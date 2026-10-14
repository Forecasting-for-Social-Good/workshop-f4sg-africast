--[[
live-questions.lua: turn `.poll`, `.wordcloud` and `.likert` divs into live-questions
questions.

For revealjs output, each question div becomes

  <div id="ID" class="lq-question lq-poll" data-lq="{...Question, correct, results}">
    ...non-list content of the div (kept as-is)...
    <ul>...the original options, task-list markers stripped...</ul>
  </div>

and the resolved `live-questions:` config is written into
<script type="application/json" id="lq-config">. The deck plugin
(live-questions.js) does the rest. For any other format the div's list is kept
as a plain list. Limits mirror LIMITS in src/shared/protocol.ts.
]]

local LIMITS = {
  promptChars = 500,
  pollOptions = 26,
  optionChars = 200,
  maxWordsCeiling = 10,
  maxAnswersCeiling = 10,
  likertStatements = 6,
  likertStatementChars = 200,
  likertPoints = 11,
  likertLabelChars = 40,
  idMaxChars = 64,
  sessionMaxChars = 64,
}
local DEFAULTS = { maxWords = 3, maxAnswers = 3, scale = "agree-5" }

-- Likert presets, resolved here so the server only ever sees explicit labels.
-- Numeric scales ("0-10", "1-5", ...) are parsed in likert_scale().
local SCALES = {
  ["agree-5"] = { "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree" },
  ["agree-7"] = {
    "Strongly disagree", "Disagree", "Somewhat disagree", "Neutral",
    "Somewhat agree", "Agree", "Strongly agree",
  },
}

local CHECKED = "\u{2612}"   -- ☒  Pandoc's task-list "[x]"
local UNCHECKED = "\u{2610}" -- ☐  Pandoc's task-list "[ ]"

local is_reveal = quarto.doc.is_format("revealjs")

local config = {
  server = nil,
  session = nil,
  closeOnLeave = true,
  results = "live",
  qr = true,
}
local seen_ids = {}
local last_heading = ""
local question_count = 0

local function warn(msg)
  quarto.log.warning("[live-questions] " .. msg)
end

local function ulen(s)
  return utf8.len(s) or #s
end

--- Cut a string to at most n UTF-8 characters.
local function utruncate(s, n)
  if ulen(s) <= n then return s end
  local stop = utf8.offset(s, n + 1)
  return stop and s:sub(1, stop - 1) or s
end

local function trim(s)
  return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function valid_session(s)
  return #s <= LIMITS.sessionMaxChars and s:match("^[A-Za-z0-9][A-Za-z0-9_-]*$") ~= nil
end

local function valid_id(s)
  return #s <= LIMITS.idMaxChars and s:match("^[A-Za-z0-9][A-Za-z0-9_:.-]*$") ~= nil
end

--- Session name from the input file stem, made to match LIMITS.sessionPattern.
local function default_session()
  local input = quarto.doc.input_file or (PANDOC_STATE.input_files or {})[1] or "deck"
  local stem = pandoc.path.split_extension(pandoc.path.filename(input))
  local s = stem:gsub("[^A-Za-z0-9_-]", "-"):gsub("%-%-+", "-"):gsub("^[^A-Za-z0-9]+", "")
  s = s:sub(1, LIMITS.sessionMaxChars):gsub("%-+$", "")
  if s == "" then s = "deck" end
  return s
end

local function meta_string(v)
  if v == nil then return nil end
  if type(v) == "boolean" then return tostring(v) end
  return trim(pandoc.utils.stringify(v))
end

local function meta_bool(v, default, key)
  if v == nil then return default end
  if type(v) == "boolean" then return v end
  local s = meta_string(v):lower()
  if s == "true" or s == "yes" then return true end
  if s == "false" or s == "no" then return false end
  warn(("live-questions.%s should be true or false, got %q; using %s"):format(key, s, tostring(default)))
  return default
end

local function results_mode(v, where)
  if v == "live" or v == "hidden" then return v end
  warn(("%s: results should be \"live\" or \"hidden\", got %q; using \"live\""):format(where, tostring(v)))
  return "live"
end

--- The options map, read from `extensions: live-questions:` (the form Quarto
--- Wizard completes) and from top-level `live-questions:`. A key set in both
--- places takes the top-level value.
local function options_map(meta)
  local function as_map(v, name)
    if v == nil then return {} end
    if pandoc.utils.type(v) ~= "table" then
      warn(("`%s:` should be a map of options"):format(name))
      return {}
    end
    return v
  end

  local ext = meta.extensions
  local nested = nil
  if ext ~= nil and pandoc.utils.type(ext) == "table" then
    nested = ext["live-questions"]
  end

  local m = {}
  for k, v in pairs(as_map(nested, "extensions: live-questions")) do m[k] = v end
  for k, v in pairs(as_map(meta["live-questions"], "live-questions")) do m[k] = v end
  return m
end

local function read_config(meta)
  local m = options_map(meta)

  local server = meta_string(m.server)
  if server and server ~= "" then
    server = server:gsub("/+$", "")
    if not server:match("^https?://[^/]+$") then
      warn(("live-questions.server should be an origin like https://host[:port], got %q"):format(server))
    end
    config.server = server
  end

  local session = meta_string(m.session)
  if session == nil or session == "" then
    session = default_session()
  elseif not valid_session(session) then
    local fixed = default_session()
    warn(("live-questions.session %q must match [A-Za-z0-9][A-Za-z0-9_-]{0,63}; using %q"):format(session, fixed))
    session = fixed
  end
  config.session = session

  config.closeOnLeave = meta_bool(m["close-on-leave"], true, "close-on-leave")
  config.qr = meta_bool(m.qr, true, "qr")
  if m.results ~= nil then
    config.results = results_mode(meta_string(m.results), "live-questions")
  end
end

--- If a list item starts with a task-list marker, return (checked, item without it).
local function strip_marker(item)
  local first = item[1]
  if first == nil or (first.t ~= "Plain" and first.t ~= "Para") then return nil, item end
  local inl = first.content
  local head = inl[1]
  if head == nil or head.t ~= "Str" then return nil, item end
  local checked
  if head.text == CHECKED then checked = true
  elseif head.text == UNCHECKED then checked = false
  else return nil, item end
  local rest = pandoc.List()
  local start = 2
  if inl[2] and inl[2].t == "Space" then start = 3 end
  for i = start, #inl do rest:insert(inl[i]) end
  local copy = item:clone()
  copy[1] = first.t == "Plain" and pandoc.Plain(rest) or pandoc.Para(rest)
  return checked, copy
end

local function is_list(b)
  return b.t == "BulletList" or b.t == "OrderedList"
end

local function int_attr(attrs, name, default, ceiling, where)
  local raw = attrs[name]
  if raw == nil then return default end
  local n = tonumber(raw)
  if n == nil or n ~= math.floor(n) then
    warn(("%s: %s=%q is not an integer; using %d"):format(where, name, raw, default))
    return default
  end
  if n < 1 or n > ceiling then
    local clamped = math.max(1, math.min(ceiling, n))
    warn(("%s: %s=%d is outside 1..%d; using %d"):format(where, name, n, ceiling, clamped))
    return clamped
  end
  return n
end

--- A short attribute value cut to n characters, with a warning.
local function short_text(s, n, what, where)
  s = trim((s or ""):gsub("%s+", " "))
  if ulen(s) > n then
    warn(("%s: %s is longer than %d characters; truncated"):format(where, what, n))
    s = utruncate(s, n)
  end
  return s
end

--- Resolve a likert div's scale attributes to (labels, numeric, anchors).
local function likert_scale(attrs, where)
  local labels, numeric = nil, false
  if attrs.labels ~= nil then
    if attrs.scale ~= nil then
      warn(("%s: both scale= and labels= given; using labels="):format(where))
    end
    labels = {}
    for part in (attrs.labels .. "|"):gmatch("([^|]*)|") do
      local label = short_text(part, LIMITS.likertLabelChars, "a scale label", where)
      if label ~= "" then table.insert(labels, label) end
    end
    if #labels < 2 or #labels > LIMITS.likertPoints then
      warn(("%s: labels= needs 2 to %d labels separated by |, found %d; using %s")
        :format(where, LIMITS.likertPoints, #labels, DEFAULTS.scale))
      labels = nil
    end
  else
    local scale = trim(attrs.scale or DEFAULTS.scale)
    local lo, hi = scale:match("^(%d+)%s*%-%s*(%d+)$")
    if SCALES[scale] then
      labels = SCALES[scale]
    elseif lo then
      lo, hi = tonumber(lo), tonumber(hi)
      local points = hi - lo + 1
      if points < 2 or points > LIMITS.likertPoints then
        warn(("%s: scale=%q must span 2 to %d points (e.g. 0-10, 1-5); using %s")
          :format(where, scale, LIMITS.likertPoints, DEFAULTS.scale))
      else
        labels, numeric = {}, true
        for i = lo, hi do table.insert(labels, tostring(i)) end
      end
    else
      warn(("%s: unknown scale=%q (use agree-5, agree-7, a range like 0-10, or labels=\"a|b|c\"); using %s")
        :format(where, scale, DEFAULTS.scale))
    end
  end
  labels = labels or SCALES[DEFAULTS.scale]

  local anchors = nil
  if attrs.low ~= nil or attrs.high ~= nil then
    if numeric then
      anchors = {
        short_text(attrs.low, LIMITS.likertLabelChars, "low=", where),
        short_text(attrs.high, LIMITS.likertLabelChars, "high=", where),
      }
    else
      warn(("%s: low= and high= only apply to numeric scales; ignored"):format(where))
    end
  end
  return labels, numeric, anchors
end

local function build_question(div, kind)
  local attrs = div.attributes
  local where = div.identifier ~= "" and ("#" .. div.identifier) or ("." .. kind .. " after \"" .. last_heading .. "\"")

  -- Split the div into its (first) list and everything else.
  local list, other = nil, pandoc.List()
  for _, b in ipairs(div.content) do
    if list == nil and is_list(b) and kind ~= "wordcloud" then
      list = b
    else
      other:insert(b)
    end
  end

  -- Options, correct answers, and the cleaned list.
  local options, correct = {}, {}
  local cleaned
  if list then
    local items = pandoc.List()
    for i, item in ipairs(list.content) do
      local checked, clean = strip_marker(item)
      if checked and kind == "poll" then table.insert(correct, i - 1) end
      if checked ~= nil and kind == "likert" then
        warn(("%s: task-list markers have no meaning in a .likert list; ignored"):format(where))
      end
      items:insert(clean)
      local label = trim(pandoc.utils.stringify(pandoc.Div(clean)))
      local max = kind == "likert" and LIMITS.likertStatementChars or LIMITS.optionChars
      if ulen(label) > max then
        warn(("%s: %s %d is longer than %d characters; truncated")
          :format(where, kind == "likert" and "statement" or "option", i, max))
        label = utruncate(label, max)
      end
      table.insert(options, label)
    end
    if list.t == "BulletList" then
      cleaned = pandoc.BulletList(items)
    else
      cleaned = pandoc.OrderedList(items, list.listAttributes)
    end
  end

  -- Prompt: attribute, else non-list text in the div, else preceding heading.
  local prompt = attrs.prompt
  if prompt == nil or trim(prompt) == "" then
    prompt = trim(pandoc.utils.stringify(other))
  end
  if prompt == "" then prompt = last_heading end
  prompt = trim((prompt or ""):gsub("%s+", " "))
  if prompt == "" then
    warn(("%s: no prompt (no prompt= attribute, text in the div, or preceding heading)"):format(where))
  end
  if ulen(prompt) > LIMITS.promptChars then
    warn(("%s: prompt is longer than %d characters; truncated"):format(where, LIMITS.promptChars))
    prompt = utruncate(prompt, LIMITS.promptChars)
  end

  -- Likert: the list items are statements on one shared scale. Without a
  -- list, the prompt is the single statement.
  local labels, numeric, anchors
  if kind == "likert" then
    labels, numeric, anchors = likert_scale(attrs, where)
    if #options == 0 and prompt ~= "" then
      options = { utruncate(prompt, LIMITS.likertStatementChars) }
    end
    if #options > LIMITS.likertStatements then
      warn(("%s: a likert question may have at most %d statements, found %d")
        :format(where, LIMITS.likertStatements, #options))
    end
  end

  -- Id: explicit, else generated from prompt and options (and the scale).
  local id = div.identifier
  if id == "" then
    local basis = prompt .. "\n" .. table.concat(options, "\n")
    if labels then basis = basis .. "\n" .. table.concat(labels, "|") end
    id = "q-" .. pandoc.utils.sha1(basis):sub(1, 10)
  elseif not valid_id(id) then
    warn(("question id %q must match [A-Za-z0-9][A-Za-z0-9_:.-]{0,63}; the server will reject it"):format(id))
  end
  if seen_ids[id] then
    warn(("duplicate question id %q (%s); give each question its own #id"):format(id, where))
  end
  seen_ids[id] = true

  local results = config.results
  if attrs.results ~= nil then
    results = results_mode(attrs.results, where)
  end

  local q
  if kind == "poll" then
    if #options < 2 then
      warn(("%s: a poll needs at least two options, found %d"):format(where, #options))
    end
    if #options > LIMITS.pollOptions then
      warn(("%s: a poll may have at most %d options, found %d"):format(where, LIMITS.pollOptions, #options))
    end
    q = { type = "poll", id = id, prompt = prompt, options = options, results = results }
    if #correct > 0 then q.correct = correct end
  elseif kind == "likert" then
    q = {
      type = "likert",
      id = id,
      prompt = prompt,
      statements = options,
      labels = labels,
      numeric = numeric,
      anchors = anchors or pandoc.json.null,
      results = results,
    }
  else
    q = {
      type = "wordcloud",
      id = id,
      prompt = prompt,
      maxWords = int_attr(attrs, "max-words", DEFAULTS.maxWords, LIMITS.maxWordsCeiling, where),
      maxAnswers = int_attr(attrs, "max-answers", DEFAULTS.maxAnswers, LIMITS.maxAnswersCeiling, where),
      results = results,
    }
  end
  return q, other, cleaned
end

local function transform(div, kind)
  question_count = question_count + 1
  local q, other, cleaned = build_question(div, kind)

  -- A likert question's static form names its scale under the statements.
  local scale_note = nil
  if kind == "likert" then
    local text = "Scale: " .. table.concat(q.labels, " \u{00b7} ")
    if q.anchors ~= pandoc.json.null then
      text = ("Scale: %s (%s) to %s (%s)"):format(q.labels[1], q.anchors[1], q.labels[#q.labels], q.anchors[2])
    end
    scale_note = pandoc.Para({ pandoc.Emph(pandoc.Inlines(text)) })
  end

  if not is_reveal then
    -- Other formats: the prompt text (if any) and a plain list.
    local blocks = pandoc.List(other)
    if cleaned then blocks:insert(cleaned) end
    if scale_note then blocks:insert(scale_note) end
    return blocks
  end

  local content = pandoc.List()
  if #other > 0 then
    content:insert(pandoc.Div(other, pandoc.Attr("", { "lq-text" })))
  end
  if cleaned then content:insert(cleaned) end
  if scale_note then content:insert(pandoc.Div({ scale_note }, pandoc.Attr("", { "lq-scale-note" }))) end

  local classes = { "lq-question", "lq-" .. kind }
  for _, c in ipairs(div.classes) do
    if c ~= kind then table.insert(classes, c) end
  end
  local json = pandoc.json.encode(q)
  return pandoc.Div(content, pandoc.Attr(q.id, classes, { ["data-lq"] = json }))
end

local function config_script()
  local out = {
    session = config.session,
    closeOnLeave = config.closeOnLeave,
    results = config.results,
    qr = config.qr,
  }
  if config.server then out.server = config.server end
  -- Keep "</script>" inside a JSON string from closing the element.
  local json = pandoc.json.encode(out):gsub("</", "<\\/")
  return '<script type="application/json" id="lq-config">' .. json .. "</script>"
end

return {
  {
    Meta = function(meta)
      read_config(meta)
    end,
  },
  {
    Pandoc = function(doc)
      doc.blocks = doc.blocks:walk({
        traverse = "topdown",
        Header = function(h)
          last_heading = trim(pandoc.utils.stringify(h.content))
        end,
        Div = function(div)
          local kind
          if div.classes:includes("poll") then kind = "poll"
          elseif div.classes:includes("wordcloud") then kind = "wordcloud"
          elseif div.classes:includes("likert") then kind = "likert"
          else return nil end
          -- Return false so the walk does not descend into the result.
          return transform(div, kind), false
        end,
      })
      if is_reveal then
        quarto.doc.include_text("in-header", config_script())
      end
      return doc
    end,
  },
}
