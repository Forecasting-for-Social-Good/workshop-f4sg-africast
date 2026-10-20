/*! live-questions | MIT | https://github.com/seandavi/quarto-revealjs-live-questions | includes qrcode-generator, (c) 2009 Kazuhiko Arase, MIT: see THIRD_PARTY_NOTICES.md */
(() => {

  // src/shared/protocol.ts
  var PROTOCOL_VERSION = 1;
  var LIMITS = {
    sessionPattern: /^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$/,
    questionIdPattern: /^[A-Za-z0-9][A-Za-z0-9_:.-]{0,63}$/,
    promptChars: 500,
    pollOptions: 26,
    optionChars: 200,
    answerInputChars: 200,
    answerChars: 40,
    maxWordsCeiling: 10,
    maxAnswersCeiling: 10,
    cloudWords: 100,
    likertStatements: 6,
    likertStatementChars: 200,
    likertPoints: 11,
    likertLabelChars: 40
  };
  var audiencePath = (session) => `/s/${encodeURIComponent(session)}`;
  var socketPath = (session) => `/api/s/${encodeURIComponent(session)}/ws`;

  // src/deck/config.ts
  function readConfig(doc = document) {
    const el = doc.getElementById("lq-config");
    if (!el?.textContent)
      return null;
    try {
      const raw = JSON.parse(el.textContent);
      return {
        server: typeof raw.server === "string" && raw.server ? raw.server.replace(/\/+$/, "") : undefined,
        session: typeof raw.session === "string" ? raw.session : "deck",
        closeOnLeave: raw.closeOnLeave !== false,
        results: raw.results === "hidden" ? "hidden" : "live",
        qr: raw.qr !== false
      };
    } catch (err) {
      console.warn("[live-questions] could not parse #lq-config", err);
      return null;
    }
  }
  function sessionOverride(search) {
    const value = new URLSearchParams(search).get("session");
    if (value === null || value === "")
      return null;
    if (!LIMITS.sessionPattern.test(value)) {
      console.warn(`[live-questions] ignoring ?session=${value}: it must match ${LIMITS.sessionPattern}`);
      return null;
    }
    return value;
  }
  function isEmbedded(win = window) {
    if (new URLSearchParams(win.location.search).has("receiver"))
      return true;
    try {
      return win.self !== win.top;
    } catch {
      return true;
    }
  }
  function isPrint(search) {
    return /print-pdf/i.test(search);
  }
  function readQuestion(el) {
    const raw = el.dataset.lq;
    if (!raw)
      return null;
    try {
      const q = JSON.parse(raw);
      if (q.type !== "poll" && q.type !== "wordcloud" && q.type !== "likert")
        return null;
      return q;
    } catch (err) {
      console.warn(`[live-questions] bad data-lq on #${el.id}`, err);
      return null;
    }
  }
  function definition(q) {
    switch (q.type) {
      case "poll":
        return { type: q.type, id: q.id, prompt: q.prompt, options: q.options };
      case "wordcloud":
        return { type: q.type, id: q.id, prompt: q.prompt, maxWords: q.maxWords, maxAnswers: q.maxAnswers };
      case "likert":
        return {
          type: q.type,
          id: q.id,
          prompt: q.prompt,
          statements: q.statements,
          labels: q.labels,
          numeric: q.numeric,
          anchors: q.anchors ?? null
        };
    }
  }
  function audienceUrl(server, path) {
    return server + path;
  }
  function shortUrl(url) {
    return url.replace(/^https?:\/\//, "");
  }
  function socketUrl(server, path) {
    return server.replace(/^http/, "ws") + path;
  }

  // src/deck/connection.ts
  var PING = '{"t":"ping"}';
  var HEARTBEAT_MS = 25000;
  var STALE_MS = 2 * HEARTBEAT_MS + 1e4;
  var BACKOFF_MIN_MS = 1000;
  var BACKOFF_MAX_MS = 30000;

  class Connection {
    url;
    handlers;
    ws = null;
    attempts = 0;
    retryTimer;
    heartbeat;
    lastHeard = 0;
    stopped = false;
    welcomed = false;
    constructor(url, handlers) {
      this.url = url;
      this.handlers = handlers;
      window.addEventListener("online", () => this.reconnectNow());
    }
    start() {
      this.stopped = false;
      this.connect();
    }
    stop() {
      this.stopped = true;
      clearTimeout(this.retryTimer);
      this.ws?.close();
    }
    send(msg) {
      if (!this.ws || this.ws.readyState !== WebSocket.OPEN || !this.welcomed)
        return false;
      this.ws.send(JSON.stringify(msg));
      return true;
    }
    connect() {
      if (this.stopped)
        return;
      this.handlers.status("connecting");
      let ws;
      try {
        ws = new WebSocket(this.url);
      } catch (err) {
        console.warn("[live-questions] cannot open socket", err);
        this.scheduleRetry();
        return;
      }
      this.ws = ws;
      this.welcomed = false;
      ws.addEventListener("open", () => {
        this.lastHeard = Date.now();
        ws.send(JSON.stringify(this.handlers.hello()));
        clearInterval(this.heartbeat);
        this.heartbeat = setInterval(() => this.beat(ws), HEARTBEAT_MS);
      });
      ws.addEventListener("message", (event) => {
        this.lastHeard = Date.now();
        if (typeof event.data !== "string")
          return;
        let msg;
        try {
          msg = JSON.parse(event.data);
        } catch {
          return;
        }
        if (msg.t === "pong")
          return;
        if (msg.t === "welcome") {
          this.welcomed = true;
          this.attempts = 0;
          this.handlers.status("online");
        }
        if (msg.t === "error" && msg.code === "version") {
          console.error(`[live-questions] server speaks a different protocol version: ${msg.message}`);
          this.stop();
        }
        this.handlers.message(msg);
      });
      ws.addEventListener("close", () => {
        if (this.ws !== ws)
          return;
        clearInterval(this.heartbeat);
        this.ws = null;
        this.welcomed = false;
        this.handlers.status("offline");
        this.scheduleRetry();
      });
    }
    beat(ws) {
      if (ws.readyState !== WebSocket.OPEN)
        return;
      if (Date.now() - this.lastHeard > STALE_MS) {
        ws.close();
        return;
      }
      ws.send(PING);
    }
    scheduleRetry() {
      if (this.stopped)
        return;
      clearTimeout(this.retryTimer);
      const base = Math.min(BACKOFF_MAX_MS, BACKOFF_MIN_MS * 2 ** this.attempts);
      const delay = base / 2 + Math.random() * (base / 2);
      this.attempts++;
      this.retryTimer = setTimeout(() => this.connect(), delay);
    }
    reconnectNow() {
      if (this.stopped || this.ws)
        return;
      clearTimeout(this.retryTimer);
      this.attempts = 0;
      this.connect();
    }
  }

  // src/deck/dom.ts
  function h(tag, props = {}, ...children) {
    const el = document.createElement(tag);
    if (props.class)
      el.className = props.class;
    if (props.text !== undefined)
      el.textContent = props.text;
    if (props.attrs)
      for (const [k, v] of Object.entries(props.attrs))
        el.setAttribute(k, v);
    for (const c of children) {
      if (c === null || c === undefined || c === false)
        continue;
      el.append(c);
    }
    return el;
  }
  function button(text, onClick, cls = "") {
    const b = h("button", { class: `lq-btn ${cls}`.trim(), text, attrs: { type: "button" } });
    b.addEventListener("click", (e) => {
      e.stopPropagation();
      onClick();
    });
    return b;
  }

  // node_modules/qrcode-generator/dist/qrcode.mjs
  var qrcode = function(typeNumber, errorCorrectionLevel) {
    const PAD0 = 236;
    const PAD1 = 17;
    let _typeNumber = typeNumber;
    const _errorCorrectionLevel = QRErrorCorrectionLevel[errorCorrectionLevel];
    let _modules = null;
    let _moduleCount = 0;
    let _dataCache = null;
    const _dataList = [];
    const _this = {};
    const makeImpl = function(test, maskPattern) {
      _moduleCount = _typeNumber * 4 + 17;
      _modules = function(moduleCount) {
        const modules = new Array(moduleCount);
        for (let row = 0;row < moduleCount; row += 1) {
          modules[row] = new Array(moduleCount);
          for (let col = 0;col < moduleCount; col += 1) {
            modules[row][col] = null;
          }
        }
        return modules;
      }(_moduleCount);
      setupPositionProbePattern(0, 0);
      setupPositionProbePattern(_moduleCount - 7, 0);
      setupPositionProbePattern(0, _moduleCount - 7);
      setupPositionAdjustPattern();
      setupTimingPattern();
      setupTypeInfo(test, maskPattern);
      if (_typeNumber >= 7) {
        setupTypeNumber(test);
      }
      if (_dataCache == null) {
        _dataCache = createData(_typeNumber, _errorCorrectionLevel, _dataList);
      }
      mapData(_dataCache, maskPattern);
    };
    const setupPositionProbePattern = function(row, col) {
      for (let r = -1;r <= 7; r += 1) {
        if (row + r <= -1 || _moduleCount <= row + r)
          continue;
        for (let c = -1;c <= 7; c += 1) {
          if (col + c <= -1 || _moduleCount <= col + c)
            continue;
          if (0 <= r && r <= 6 && (c == 0 || c == 6) || 0 <= c && c <= 6 && (r == 0 || r == 6) || 2 <= r && r <= 4 && 2 <= c && c <= 4) {
            _modules[row + r][col + c] = true;
          } else {
            _modules[row + r][col + c] = false;
          }
        }
      }
    };
    const getBestMaskPattern = function() {
      let minLostPoint = 0;
      let pattern = 0;
      for (let i = 0;i < 8; i += 1) {
        makeImpl(true, i);
        const lostPoint = QRUtil.getLostPoint(_this);
        if (i == 0 || minLostPoint > lostPoint) {
          minLostPoint = lostPoint;
          pattern = i;
        }
      }
      return pattern;
    };
    const setupTimingPattern = function() {
      for (let r = 8;r < _moduleCount - 8; r += 1) {
        if (_modules[r][6] != null) {
          continue;
        }
        _modules[r][6] = r % 2 == 0;
      }
      for (let c = 8;c < _moduleCount - 8; c += 1) {
        if (_modules[6][c] != null) {
          continue;
        }
        _modules[6][c] = c % 2 == 0;
      }
    };
    const setupPositionAdjustPattern = function() {
      const pos = QRUtil.getPatternPosition(_typeNumber);
      for (let i = 0;i < pos.length; i += 1) {
        for (let j = 0;j < pos.length; j += 1) {
          const row = pos[i];
          const col = pos[j];
          if (_modules[row][col] != null) {
            continue;
          }
          for (let r = -2;r <= 2; r += 1) {
            for (let c = -2;c <= 2; c += 1) {
              if (r == -2 || r == 2 || c == -2 || c == 2 || r == 0 && c == 0) {
                _modules[row + r][col + c] = true;
              } else {
                _modules[row + r][col + c] = false;
              }
            }
          }
        }
      }
    };
    const setupTypeNumber = function(test) {
      const bits = QRUtil.getBCHTypeNumber(_typeNumber);
      for (let i = 0;i < 18; i += 1) {
        const mod = !test && (bits >> i & 1) == 1;
        _modules[Math.floor(i / 3)][i % 3 + _moduleCount - 8 - 3] = mod;
      }
      for (let i = 0;i < 18; i += 1) {
        const mod = !test && (bits >> i & 1) == 1;
        _modules[i % 3 + _moduleCount - 8 - 3][Math.floor(i / 3)] = mod;
      }
    };
    const setupTypeInfo = function(test, maskPattern) {
      const data = _errorCorrectionLevel << 3 | maskPattern;
      const bits = QRUtil.getBCHTypeInfo(data);
      for (let i = 0;i < 15; i += 1) {
        const mod = !test && (bits >> i & 1) == 1;
        if (i < 6) {
          _modules[i][8] = mod;
        } else if (i < 8) {
          _modules[i + 1][8] = mod;
        } else {
          _modules[_moduleCount - 15 + i][8] = mod;
        }
      }
      for (let i = 0;i < 15; i += 1) {
        const mod = !test && (bits >> i & 1) == 1;
        if (i < 8) {
          _modules[8][_moduleCount - i - 1] = mod;
        } else if (i < 9) {
          _modules[8][15 - i - 1 + 1] = mod;
        } else {
          _modules[8][15 - i - 1] = mod;
        }
      }
      _modules[_moduleCount - 8][8] = !test;
    };
    const mapData = function(data, maskPattern) {
      let inc = -1;
      let row = _moduleCount - 1;
      let bitIndex = 7;
      let byteIndex = 0;
      const maskFunc = QRUtil.getMaskFunction(maskPattern);
      for (let col = _moduleCount - 1;col > 0; col -= 2) {
        if (col == 6)
          col -= 1;
        while (true) {
          for (let c = 0;c < 2; c += 1) {
            if (_modules[row][col - c] == null) {
              let dark = false;
              if (byteIndex < data.length) {
                dark = (data[byteIndex] >>> bitIndex & 1) == 1;
              }
              const mask = maskFunc(row, col - c);
              if (mask) {
                dark = !dark;
              }
              _modules[row][col - c] = dark;
              bitIndex -= 1;
              if (bitIndex == -1) {
                byteIndex += 1;
                bitIndex = 7;
              }
            }
          }
          row += inc;
          if (row < 0 || _moduleCount <= row) {
            row -= inc;
            inc = -inc;
            break;
          }
        }
      }
    };
    const createBytes = function(buffer, rsBlocks) {
      let offset = 0;
      let maxDcCount = 0;
      let maxEcCount = 0;
      const dcdata = new Array(rsBlocks.length);
      const ecdata = new Array(rsBlocks.length);
      for (let r = 0;r < rsBlocks.length; r += 1) {
        const dcCount = rsBlocks[r].dataCount;
        const ecCount = rsBlocks[r].totalCount - dcCount;
        maxDcCount = Math.max(maxDcCount, dcCount);
        maxEcCount = Math.max(maxEcCount, ecCount);
        dcdata[r] = new Array(dcCount);
        for (let i = 0;i < dcdata[r].length; i += 1) {
          dcdata[r][i] = 255 & buffer.getBuffer()[i + offset];
        }
        offset += dcCount;
        const rsPoly = QRUtil.getErrorCorrectPolynomial(ecCount);
        const rawPoly = qrPolynomial(dcdata[r], rsPoly.getLength() - 1);
        const modPoly = rawPoly.mod(rsPoly);
        ecdata[r] = new Array(rsPoly.getLength() - 1);
        for (let i = 0;i < ecdata[r].length; i += 1) {
          const modIndex = i + modPoly.getLength() - ecdata[r].length;
          ecdata[r][i] = modIndex >= 0 ? modPoly.getAt(modIndex) : 0;
        }
      }
      let totalCodeCount = 0;
      for (let i = 0;i < rsBlocks.length; i += 1) {
        totalCodeCount += rsBlocks[i].totalCount;
      }
      const data = new Array(totalCodeCount);
      let index = 0;
      for (let i = 0;i < maxDcCount; i += 1) {
        for (let r = 0;r < rsBlocks.length; r += 1) {
          if (i < dcdata[r].length) {
            data[index] = dcdata[r][i];
            index += 1;
          }
        }
      }
      for (let i = 0;i < maxEcCount; i += 1) {
        for (let r = 0;r < rsBlocks.length; r += 1) {
          if (i < ecdata[r].length) {
            data[index] = ecdata[r][i];
            index += 1;
          }
        }
      }
      return data;
    };
    const createData = function(typeNumber2, errorCorrectionLevel2, dataList) {
      const rsBlocks = QRRSBlock.getRSBlocks(typeNumber2, errorCorrectionLevel2);
      const buffer = qrBitBuffer();
      for (let i = 0;i < dataList.length; i += 1) {
        const data = dataList[i];
        buffer.put(data.getMode(), 4);
        buffer.put(data.getLength(), QRUtil.getLengthInBits(data.getMode(), typeNumber2));
        data.write(buffer);
      }
      let totalDataCount = 0;
      for (let i = 0;i < rsBlocks.length; i += 1) {
        totalDataCount += rsBlocks[i].dataCount;
      }
      if (buffer.getLengthInBits() > totalDataCount * 8) {
        throw "code length overflow. (" + buffer.getLengthInBits() + ">" + totalDataCount * 8 + ")";
      }
      if (buffer.getLengthInBits() + 4 <= totalDataCount * 8) {
        buffer.put(0, 4);
      }
      while (buffer.getLengthInBits() % 8 != 0) {
        buffer.putBit(false);
      }
      while (true) {
        if (buffer.getLengthInBits() >= totalDataCount * 8) {
          break;
        }
        buffer.put(PAD0, 8);
        if (buffer.getLengthInBits() >= totalDataCount * 8) {
          break;
        }
        buffer.put(PAD1, 8);
      }
      return createBytes(buffer, rsBlocks);
    };
    _this.addData = function(data, mode) {
      mode = mode || "Byte";
      let newData = null;
      switch (mode) {
        case "Numeric":
          newData = qrNumber(data);
          break;
        case "Alphanumeric":
          newData = qrAlphaNum(data);
          break;
        case "Byte":
          newData = qr8BitByte(data);
          break;
        case "Kanji":
          newData = qrKanji(data);
          break;
        default:
          throw "mode:" + mode;
      }
      _dataList.push(newData);
      _dataCache = null;
    };
    _this.isDark = function(row, col) {
      if (row < 0 || _moduleCount <= row || col < 0 || _moduleCount <= col) {
        throw row + "," + col;
      }
      return _modules[row][col];
    };
    _this.getModuleCount = function() {
      return _moduleCount;
    };
    _this.make = function() {
      if (_typeNumber < 1) {
        let typeNumber2 = 1;
        for (;typeNumber2 < 40; typeNumber2++) {
          const rsBlocks = QRRSBlock.getRSBlocks(typeNumber2, _errorCorrectionLevel);
          const buffer = qrBitBuffer();
          for (let i = 0;i < _dataList.length; i++) {
            const data = _dataList[i];
            buffer.put(data.getMode(), 4);
            buffer.put(data.getLength(), QRUtil.getLengthInBits(data.getMode(), typeNumber2));
            data.write(buffer);
          }
          let totalDataCount = 0;
          for (let i = 0;i < rsBlocks.length; i++) {
            totalDataCount += rsBlocks[i].dataCount;
          }
          if (buffer.getLengthInBits() <= totalDataCount * 8) {
            break;
          }
        }
        _typeNumber = typeNumber2;
      }
      makeImpl(false, getBestMaskPattern());
    };
    _this.createTableTag = function(cellSize, margin) {
      cellSize = cellSize || 2;
      margin = typeof margin == "undefined" ? cellSize * 4 : margin;
      let qrHtml = "";
      qrHtml += '<table style="';
      qrHtml += " border-width: 0px; border-style: none;";
      qrHtml += " border-collapse: collapse;";
      qrHtml += " padding: 0px; margin: " + margin + "px;";
      qrHtml += '">';
      qrHtml += "<tbody>";
      for (let r = 0;r < _this.getModuleCount(); r += 1) {
        qrHtml += "<tr>";
        for (let c = 0;c < _this.getModuleCount(); c += 1) {
          qrHtml += '<td style="';
          qrHtml += " border-width: 0px; border-style: none;";
          qrHtml += " border-collapse: collapse;";
          qrHtml += " padding: 0px; margin: 0px;";
          qrHtml += " width: " + cellSize + "px;";
          qrHtml += " height: " + cellSize + "px;";
          qrHtml += " background-color: ";
          qrHtml += _this.isDark(r, c) ? "#000000" : "#ffffff";
          qrHtml += ";";
          qrHtml += '"/>';
        }
        qrHtml += "</tr>";
      }
      qrHtml += "</tbody>";
      qrHtml += "</table>";
      return qrHtml;
    };
    _this.createSvgTag = function(cellSize, margin, alt, title) {
      let opts = {};
      if (typeof arguments[0] == "object") {
        opts = arguments[0];
        cellSize = opts.cellSize;
        margin = opts.margin;
        alt = opts.alt;
        title = opts.title;
      }
      cellSize = cellSize || 2;
      margin = typeof margin == "undefined" ? cellSize * 4 : margin;
      alt = typeof alt === "string" ? { text: alt } : alt || {};
      alt.text = alt.text || null;
      alt.id = alt.text ? alt.id || "qrcode-description" : null;
      title = typeof title === "string" ? { text: title } : title || {};
      title.text = title.text || null;
      title.id = title.text ? title.id || "qrcode-title" : null;
      const size = _this.getModuleCount() * cellSize + margin * 2;
      let c, mc, r, mr, qrSvg = "", rect;
      rect = "l" + cellSize + ",0 0," + cellSize + " -" + cellSize + ",0 0,-" + cellSize + "z ";
      qrSvg += '<svg version="1.1" xmlns="http://www.w3.org/2000/svg"';
      qrSvg += !opts.scalable ? ' width="' + size + 'px" height="' + size + 'px"' : "";
      qrSvg += ' viewBox="0 0 ' + size + " " + size + '" ';
      qrSvg += ' preserveAspectRatio="xMinYMin meet"';
      qrSvg += title.text || alt.text ? ' role="img" aria-labelledby="' + escapeXml([title.id, alt.id].join(" ").trim()) + '"' : "";
      qrSvg += ">";
      qrSvg += title.text ? '<title id="' + escapeXml(title.id) + '">' + escapeXml(title.text) + "</title>" : "";
      qrSvg += alt.text ? '<description id="' + escapeXml(alt.id) + '">' + escapeXml(alt.text) + "</description>" : "";
      qrSvg += '<rect width="100%" height="100%" fill="white" cx="0" cy="0"/>';
      qrSvg += '<path d="';
      for (r = 0;r < _this.getModuleCount(); r += 1) {
        mr = r * cellSize + margin;
        for (c = 0;c < _this.getModuleCount(); c += 1) {
          if (_this.isDark(r, c)) {
            mc = c * cellSize + margin;
            qrSvg += "M" + mc + "," + mr + rect;
          }
        }
      }
      qrSvg += '" stroke="transparent" fill="black"/>';
      qrSvg += "</svg>";
      return qrSvg;
    };
    _this.createDataURL = function(cellSize, margin) {
      cellSize = cellSize || 2;
      margin = typeof margin == "undefined" ? cellSize * 4 : margin;
      const size = _this.getModuleCount() * cellSize + margin * 2;
      const min = margin;
      const max = size - margin;
      return createDataURL(size, size, function(x, y) {
        if (min <= x && x < max && min <= y && y < max) {
          const c = Math.floor((x - min) / cellSize);
          const r = Math.floor((y - min) / cellSize);
          return _this.isDark(r, c) ? 0 : 1;
        } else {
          return 1;
        }
      });
    };
    _this.createImgTag = function(cellSize, margin, alt) {
      cellSize = cellSize || 2;
      margin = typeof margin == "undefined" ? cellSize * 4 : margin;
      const size = _this.getModuleCount() * cellSize + margin * 2;
      let img = "";
      img += "<img";
      img += ' src="';
      img += _this.createDataURL(cellSize, margin);
      img += '"';
      img += ' width="';
      img += size;
      img += '"';
      img += ' height="';
      img += size;
      img += '"';
      if (alt) {
        img += ' alt="';
        img += escapeXml(alt);
        img += '"';
      }
      img += "/>";
      return img;
    };
    const escapeXml = function(s) {
      let escaped = "";
      for (let i = 0;i < s.length; i += 1) {
        const c = s.charAt(i);
        switch (c) {
          case "<":
            escaped += "&lt;";
            break;
          case ">":
            escaped += "&gt;";
            break;
          case "&":
            escaped += "&amp;";
            break;
          case '"':
            escaped += "&quot;";
            break;
          default:
            escaped += c;
            break;
        }
      }
      return escaped;
    };
    const _createHalfASCII = function(margin) {
      const cellSize = 1;
      margin = typeof margin == "undefined" ? cellSize * 2 : margin;
      const size = _this.getModuleCount() * cellSize + margin * 2;
      const min = margin;
      const max = size - margin;
      let y, x, r1, r2, p;
      const blocks = {
        "██": "█",
        "█ ": "▀",
        " █": "▄",
        "  ": " "
      };
      const blocksLastLineNoMargin = {
        "██": "▀",
        "█ ": "▀",
        " █": " ",
        "  ": " "
      };
      let ascii = "";
      for (y = 0;y < size; y += 2) {
        r1 = Math.floor((y - min) / cellSize);
        r2 = Math.floor((y + 1 - min) / cellSize);
        for (x = 0;x < size; x += 1) {
          p = "█";
          if (min <= x && x < max && min <= y && y < max && _this.isDark(r1, Math.floor((x - min) / cellSize))) {
            p = " ";
          }
          if (min <= x && x < max && min <= y + 1 && y + 1 < max && _this.isDark(r2, Math.floor((x - min) / cellSize))) {
            p += " ";
          } else {
            p += "█";
          }
          ascii += margin < 1 && y + 1 >= max ? blocksLastLineNoMargin[p] : blocks[p];
        }
        ascii += `
`;
      }
      if (size % 2 && margin > 0) {
        return ascii.substring(0, ascii.length - size - 1) + Array(size + 1).join("▀");
      }
      return ascii.substring(0, ascii.length - 1);
    };
    _this.createASCII = function(cellSize, margin) {
      cellSize = cellSize || 1;
      if (cellSize < 2) {
        return _createHalfASCII(margin);
      }
      cellSize -= 1;
      margin = typeof margin == "undefined" ? cellSize * 2 : margin;
      const size = _this.getModuleCount() * cellSize + margin * 2;
      const min = margin;
      const max = size - margin;
      let y, x, r, p;
      const white = Array(cellSize + 1).join("██");
      const black = Array(cellSize + 1).join("  ");
      let ascii = "";
      let line = "";
      for (y = 0;y < size; y += 1) {
        r = Math.floor((y - min) / cellSize);
        line = "";
        for (x = 0;x < size; x += 1) {
          p = 1;
          if (min <= x && x < max && min <= y && y < max && _this.isDark(r, Math.floor((x - min) / cellSize))) {
            p = 0;
          }
          line += p ? white : black;
        }
        for (r = 0;r < cellSize; r += 1) {
          ascii += line + `
`;
        }
      }
      return ascii.substring(0, ascii.length - 1);
    };
    _this.renderTo2dContext = function(context, cellSize) {
      cellSize = cellSize || 2;
      const length = _this.getModuleCount();
      for (let row = 0;row < length; row++) {
        for (let col = 0;col < length; col++) {
          context.fillStyle = _this.isDark(row, col) ? "black" : "white";
          context.fillRect(col * cellSize, row * cellSize, cellSize, cellSize);
        }
      }
    };
    return _this;
  };
  qrcode.stringToBytes = function(s) {
    const bytes = [];
    for (let i = 0;i < s.length; i += 1) {
      const c = s.charCodeAt(i);
      bytes.push(c & 255);
    }
    return bytes;
  };
  qrcode.createStringToBytes = function(unicodeData, numChars) {
    const unicodeMap = function() {
      const bin = base64DecodeInputStream(unicodeData);
      const read = function() {
        const b = bin.read();
        if (b == -1)
          throw "eof";
        return b;
      };
      let count = 0;
      const unicodeMap2 = {};
      while (true) {
        const b0 = bin.read();
        if (b0 == -1)
          break;
        const b1 = read();
        const b2 = read();
        const b3 = read();
        const k = String.fromCharCode(b0 << 8 | b1);
        const v = b2 << 8 | b3;
        unicodeMap2[k] = v;
        count += 1;
      }
      if (count != numChars) {
        throw count + " != " + numChars;
      }
      return unicodeMap2;
    }();
    const unknownChar = 63;
    return function(s) {
      const bytes = [];
      for (let i = 0;i < s.length; i += 1) {
        const c = s.charCodeAt(i);
        if (c < 128) {
          bytes.push(c);
        } else {
          const b = unicodeMap[s.charAt(i)];
          if (typeof b == "number") {
            if ((b & 255) == b) {
              bytes.push(b);
            } else {
              bytes.push(b >>> 8);
              bytes.push(b & 255);
            }
          } else {
            bytes.push(unknownChar);
          }
        }
      }
      return bytes;
    };
  };
  var QRMode = {
    MODE_NUMBER: 1 << 0,
    MODE_ALPHA_NUM: 1 << 1,
    MODE_8BIT_BYTE: 1 << 2,
    MODE_KANJI: 1 << 3
  };
  var QRErrorCorrectionLevel = {
    L: 1,
    M: 0,
    Q: 3,
    H: 2
  };
  var QRMaskPattern = {
    PATTERN000: 0,
    PATTERN001: 1,
    PATTERN010: 2,
    PATTERN011: 3,
    PATTERN100: 4,
    PATTERN101: 5,
    PATTERN110: 6,
    PATTERN111: 7
  };
  var QRUtil = function() {
    const PATTERN_POSITION_TABLE = [
      [],
      [6, 18],
      [6, 22],
      [6, 26],
      [6, 30],
      [6, 34],
      [6, 22, 38],
      [6, 24, 42],
      [6, 26, 46],
      [6, 28, 50],
      [6, 30, 54],
      [6, 32, 58],
      [6, 34, 62],
      [6, 26, 46, 66],
      [6, 26, 48, 70],
      [6, 26, 50, 74],
      [6, 30, 54, 78],
      [6, 30, 56, 82],
      [6, 30, 58, 86],
      [6, 34, 62, 90],
      [6, 28, 50, 72, 94],
      [6, 26, 50, 74, 98],
      [6, 30, 54, 78, 102],
      [6, 28, 54, 80, 106],
      [6, 32, 58, 84, 110],
      [6, 30, 58, 86, 114],
      [6, 34, 62, 90, 118],
      [6, 26, 50, 74, 98, 122],
      [6, 30, 54, 78, 102, 126],
      [6, 26, 52, 78, 104, 130],
      [6, 30, 56, 82, 108, 134],
      [6, 34, 60, 86, 112, 138],
      [6, 30, 58, 86, 114, 142],
      [6, 34, 62, 90, 118, 146],
      [6, 30, 54, 78, 102, 126, 150],
      [6, 24, 50, 76, 102, 128, 154],
      [6, 28, 54, 80, 106, 132, 158],
      [6, 32, 58, 84, 110, 136, 162],
      [6, 26, 54, 82, 110, 138, 166],
      [6, 30, 58, 86, 114, 142, 170]
    ];
    const G15 = 1 << 10 | 1 << 8 | 1 << 5 | 1 << 4 | 1 << 2 | 1 << 1 | 1 << 0;
    const G18 = 1 << 12 | 1 << 11 | 1 << 10 | 1 << 9 | 1 << 8 | 1 << 5 | 1 << 2 | 1 << 0;
    const G15_MASK = 1 << 14 | 1 << 12 | 1 << 10 | 1 << 4 | 1 << 1;
    const _this = {};
    const getBCHDigit = function(data) {
      let digit = 0;
      while (data != 0) {
        digit += 1;
        data >>>= 1;
      }
      return digit;
    };
    _this.getBCHTypeInfo = function(data) {
      let d = data << 10;
      while (getBCHDigit(d) - getBCHDigit(G15) >= 0) {
        d ^= G15 << getBCHDigit(d) - getBCHDigit(G15);
      }
      return (data << 10 | d) ^ G15_MASK;
    };
    _this.getBCHTypeNumber = function(data) {
      let d = data << 12;
      while (getBCHDigit(d) - getBCHDigit(G18) >= 0) {
        d ^= G18 << getBCHDigit(d) - getBCHDigit(G18);
      }
      return data << 12 | d;
    };
    _this.getPatternPosition = function(typeNumber) {
      return PATTERN_POSITION_TABLE[typeNumber - 1];
    };
    _this.getMaskFunction = function(maskPattern) {
      switch (maskPattern) {
        case QRMaskPattern.PATTERN000:
          return function(i, j) {
            return (i + j) % 2 == 0;
          };
        case QRMaskPattern.PATTERN001:
          return function(i, j) {
            return i % 2 == 0;
          };
        case QRMaskPattern.PATTERN010:
          return function(i, j) {
            return j % 3 == 0;
          };
        case QRMaskPattern.PATTERN011:
          return function(i, j) {
            return (i + j) % 3 == 0;
          };
        case QRMaskPattern.PATTERN100:
          return function(i, j) {
            return (Math.floor(i / 2) + Math.floor(j / 3)) % 2 == 0;
          };
        case QRMaskPattern.PATTERN101:
          return function(i, j) {
            return i * j % 2 + i * j % 3 == 0;
          };
        case QRMaskPattern.PATTERN110:
          return function(i, j) {
            return (i * j % 2 + i * j % 3) % 2 == 0;
          };
        case QRMaskPattern.PATTERN111:
          return function(i, j) {
            return (i * j % 3 + (i + j) % 2) % 2 == 0;
          };
        default:
          throw "bad maskPattern:" + maskPattern;
      }
    };
    _this.getErrorCorrectPolynomial = function(errorCorrectLength) {
      let a = qrPolynomial([1], 0);
      for (let i = 0;i < errorCorrectLength; i += 1) {
        a = a.multiply(qrPolynomial([1, QRMath.gexp(i)], 0));
      }
      return a;
    };
    _this.getLengthInBits = function(mode, type) {
      if (1 <= type && type < 10) {
        switch (mode) {
          case QRMode.MODE_NUMBER:
            return 10;
          case QRMode.MODE_ALPHA_NUM:
            return 9;
          case QRMode.MODE_8BIT_BYTE:
            return 8;
          case QRMode.MODE_KANJI:
            return 8;
          default:
            throw "mode:" + mode;
        }
      } else if (type < 27) {
        switch (mode) {
          case QRMode.MODE_NUMBER:
            return 12;
          case QRMode.MODE_ALPHA_NUM:
            return 11;
          case QRMode.MODE_8BIT_BYTE:
            return 16;
          case QRMode.MODE_KANJI:
            return 10;
          default:
            throw "mode:" + mode;
        }
      } else if (type < 41) {
        switch (mode) {
          case QRMode.MODE_NUMBER:
            return 14;
          case QRMode.MODE_ALPHA_NUM:
            return 13;
          case QRMode.MODE_8BIT_BYTE:
            return 16;
          case QRMode.MODE_KANJI:
            return 12;
          default:
            throw "mode:" + mode;
        }
      } else {
        throw "type:" + type;
      }
    };
    _this.getLostPoint = function(qrcode2) {
      const moduleCount = qrcode2.getModuleCount();
      let lostPoint = 0;
      for (let row = 0;row < moduleCount; row += 1) {
        for (let col = 0;col < moduleCount; col += 1) {
          let sameCount = 0;
          const dark = qrcode2.isDark(row, col);
          for (let r = -1;r <= 1; r += 1) {
            if (row + r < 0 || moduleCount <= row + r) {
              continue;
            }
            for (let c = -1;c <= 1; c += 1) {
              if (col + c < 0 || moduleCount <= col + c) {
                continue;
              }
              if (r == 0 && c == 0) {
                continue;
              }
              if (dark == qrcode2.isDark(row + r, col + c)) {
                sameCount += 1;
              }
            }
          }
          if (sameCount > 5) {
            lostPoint += 3 + sameCount - 5;
          }
        }
      }
      for (let row = 0;row < moduleCount - 1; row += 1) {
        for (let col = 0;col < moduleCount - 1; col += 1) {
          let count = 0;
          if (qrcode2.isDark(row, col))
            count += 1;
          if (qrcode2.isDark(row + 1, col))
            count += 1;
          if (qrcode2.isDark(row, col + 1))
            count += 1;
          if (qrcode2.isDark(row + 1, col + 1))
            count += 1;
          if (count == 0 || count == 4) {
            lostPoint += 3;
          }
        }
      }
      for (let row = 0;row < moduleCount; row += 1) {
        for (let col = 0;col < moduleCount - 6; col += 1) {
          if (qrcode2.isDark(row, col) && !qrcode2.isDark(row, col + 1) && qrcode2.isDark(row, col + 2) && qrcode2.isDark(row, col + 3) && qrcode2.isDark(row, col + 4) && !qrcode2.isDark(row, col + 5) && qrcode2.isDark(row, col + 6)) {
            lostPoint += 40;
          }
        }
      }
      for (let col = 0;col < moduleCount; col += 1) {
        for (let row = 0;row < moduleCount - 6; row += 1) {
          if (qrcode2.isDark(row, col) && !qrcode2.isDark(row + 1, col) && qrcode2.isDark(row + 2, col) && qrcode2.isDark(row + 3, col) && qrcode2.isDark(row + 4, col) && !qrcode2.isDark(row + 5, col) && qrcode2.isDark(row + 6, col)) {
            lostPoint += 40;
          }
        }
      }
      let darkCount = 0;
      for (let col = 0;col < moduleCount; col += 1) {
        for (let row = 0;row < moduleCount; row += 1) {
          if (qrcode2.isDark(row, col)) {
            darkCount += 1;
          }
        }
      }
      const ratio = Math.abs(100 * darkCount / moduleCount / moduleCount - 50) / 5;
      lostPoint += ratio * 10;
      return lostPoint;
    };
    return _this;
  }();
  var QRMath = function() {
    const EXP_TABLE = new Array(256);
    const LOG_TABLE = new Array(256);
    for (let i = 0;i < 8; i += 1) {
      EXP_TABLE[i] = 1 << i;
    }
    for (let i = 8;i < 256; i += 1) {
      EXP_TABLE[i] = EXP_TABLE[i - 4] ^ EXP_TABLE[i - 5] ^ EXP_TABLE[i - 6] ^ EXP_TABLE[i - 8];
    }
    for (let i = 0;i < 255; i += 1) {
      LOG_TABLE[EXP_TABLE[i]] = i;
    }
    const _this = {};
    _this.glog = function(n) {
      if (n < 1) {
        throw "glog(" + n + ")";
      }
      return LOG_TABLE[n];
    };
    _this.gexp = function(n) {
      while (n < 0) {
        n += 255;
      }
      while (n >= 256) {
        n -= 255;
      }
      return EXP_TABLE[n];
    };
    return _this;
  }();
  var qrPolynomial = function(num, shift) {
    if (typeof num.length == "undefined") {
      throw num.length + "/" + shift;
    }
    const _num = function() {
      let offset = 0;
      while (offset < num.length && num[offset] == 0) {
        offset += 1;
      }
      const _num2 = new Array(num.length - offset + shift);
      for (let i = 0;i < num.length - offset; i += 1) {
        _num2[i] = num[i + offset];
      }
      return _num2;
    }();
    const _this = {};
    _this.getAt = function(index) {
      return _num[index];
    };
    _this.getLength = function() {
      return _num.length;
    };
    _this.multiply = function(e) {
      const num2 = new Array(_this.getLength() + e.getLength() - 1);
      for (let i = 0;i < _this.getLength(); i += 1) {
        for (let j = 0;j < e.getLength(); j += 1) {
          num2[i + j] ^= QRMath.gexp(QRMath.glog(_this.getAt(i)) + QRMath.glog(e.getAt(j)));
        }
      }
      return qrPolynomial(num2, 0);
    };
    _this.mod = function(e) {
      if (_this.getLength() - e.getLength() < 0) {
        return _this;
      }
      const ratio = QRMath.glog(_this.getAt(0)) - QRMath.glog(e.getAt(0));
      const num2 = new Array(_this.getLength());
      for (let i = 0;i < _this.getLength(); i += 1) {
        num2[i] = _this.getAt(i);
      }
      for (let i = 0;i < e.getLength(); i += 1) {
        num2[i] ^= QRMath.gexp(QRMath.glog(e.getAt(i)) + ratio);
      }
      return qrPolynomial(num2, 0).mod(e);
    };
    return _this;
  };
  var QRRSBlock = function() {
    const RS_BLOCK_TABLE = [
      [1, 26, 19],
      [1, 26, 16],
      [1, 26, 13],
      [1, 26, 9],
      [1, 44, 34],
      [1, 44, 28],
      [1, 44, 22],
      [1, 44, 16],
      [1, 70, 55],
      [1, 70, 44],
      [2, 35, 17],
      [2, 35, 13],
      [1, 100, 80],
      [2, 50, 32],
      [2, 50, 24],
      [4, 25, 9],
      [1, 134, 108],
      [2, 67, 43],
      [2, 33, 15, 2, 34, 16],
      [2, 33, 11, 2, 34, 12],
      [2, 86, 68],
      [4, 43, 27],
      [4, 43, 19],
      [4, 43, 15],
      [2, 98, 78],
      [4, 49, 31],
      [2, 32, 14, 4, 33, 15],
      [4, 39, 13, 1, 40, 14],
      [2, 121, 97],
      [2, 60, 38, 2, 61, 39],
      [4, 40, 18, 2, 41, 19],
      [4, 40, 14, 2, 41, 15],
      [2, 146, 116],
      [3, 58, 36, 2, 59, 37],
      [4, 36, 16, 4, 37, 17],
      [4, 36, 12, 4, 37, 13],
      [2, 86, 68, 2, 87, 69],
      [4, 69, 43, 1, 70, 44],
      [6, 43, 19, 2, 44, 20],
      [6, 43, 15, 2, 44, 16],
      [4, 101, 81],
      [1, 80, 50, 4, 81, 51],
      [4, 50, 22, 4, 51, 23],
      [3, 36, 12, 8, 37, 13],
      [2, 116, 92, 2, 117, 93],
      [6, 58, 36, 2, 59, 37],
      [4, 46, 20, 6, 47, 21],
      [7, 42, 14, 4, 43, 15],
      [4, 133, 107],
      [8, 59, 37, 1, 60, 38],
      [8, 44, 20, 4, 45, 21],
      [12, 33, 11, 4, 34, 12],
      [3, 145, 115, 1, 146, 116],
      [4, 64, 40, 5, 65, 41],
      [11, 36, 16, 5, 37, 17],
      [11, 36, 12, 5, 37, 13],
      [5, 109, 87, 1, 110, 88],
      [5, 65, 41, 5, 66, 42],
      [5, 54, 24, 7, 55, 25],
      [11, 36, 12, 7, 37, 13],
      [5, 122, 98, 1, 123, 99],
      [7, 73, 45, 3, 74, 46],
      [15, 43, 19, 2, 44, 20],
      [3, 45, 15, 13, 46, 16],
      [1, 135, 107, 5, 136, 108],
      [10, 74, 46, 1, 75, 47],
      [1, 50, 22, 15, 51, 23],
      [2, 42, 14, 17, 43, 15],
      [5, 150, 120, 1, 151, 121],
      [9, 69, 43, 4, 70, 44],
      [17, 50, 22, 1, 51, 23],
      [2, 42, 14, 19, 43, 15],
      [3, 141, 113, 4, 142, 114],
      [3, 70, 44, 11, 71, 45],
      [17, 47, 21, 4, 48, 22],
      [9, 39, 13, 16, 40, 14],
      [3, 135, 107, 5, 136, 108],
      [3, 67, 41, 13, 68, 42],
      [15, 54, 24, 5, 55, 25],
      [15, 43, 15, 10, 44, 16],
      [4, 144, 116, 4, 145, 117],
      [17, 68, 42],
      [17, 50, 22, 6, 51, 23],
      [19, 46, 16, 6, 47, 17],
      [2, 139, 111, 7, 140, 112],
      [17, 74, 46],
      [7, 54, 24, 16, 55, 25],
      [34, 37, 13],
      [4, 151, 121, 5, 152, 122],
      [4, 75, 47, 14, 76, 48],
      [11, 54, 24, 14, 55, 25],
      [16, 45, 15, 14, 46, 16],
      [6, 147, 117, 4, 148, 118],
      [6, 73, 45, 14, 74, 46],
      [11, 54, 24, 16, 55, 25],
      [30, 46, 16, 2, 47, 17],
      [8, 132, 106, 4, 133, 107],
      [8, 75, 47, 13, 76, 48],
      [7, 54, 24, 22, 55, 25],
      [22, 45, 15, 13, 46, 16],
      [10, 142, 114, 2, 143, 115],
      [19, 74, 46, 4, 75, 47],
      [28, 50, 22, 6, 51, 23],
      [33, 46, 16, 4, 47, 17],
      [8, 152, 122, 4, 153, 123],
      [22, 73, 45, 3, 74, 46],
      [8, 53, 23, 26, 54, 24],
      [12, 45, 15, 28, 46, 16],
      [3, 147, 117, 10, 148, 118],
      [3, 73, 45, 23, 74, 46],
      [4, 54, 24, 31, 55, 25],
      [11, 45, 15, 31, 46, 16],
      [7, 146, 116, 7, 147, 117],
      [21, 73, 45, 7, 74, 46],
      [1, 53, 23, 37, 54, 24],
      [19, 45, 15, 26, 46, 16],
      [5, 145, 115, 10, 146, 116],
      [19, 75, 47, 10, 76, 48],
      [15, 54, 24, 25, 55, 25],
      [23, 45, 15, 25, 46, 16],
      [13, 145, 115, 3, 146, 116],
      [2, 74, 46, 29, 75, 47],
      [42, 54, 24, 1, 55, 25],
      [23, 45, 15, 28, 46, 16],
      [17, 145, 115],
      [10, 74, 46, 23, 75, 47],
      [10, 54, 24, 35, 55, 25],
      [19, 45, 15, 35, 46, 16],
      [17, 145, 115, 1, 146, 116],
      [14, 74, 46, 21, 75, 47],
      [29, 54, 24, 19, 55, 25],
      [11, 45, 15, 46, 46, 16],
      [13, 145, 115, 6, 146, 116],
      [14, 74, 46, 23, 75, 47],
      [44, 54, 24, 7, 55, 25],
      [59, 46, 16, 1, 47, 17],
      [12, 151, 121, 7, 152, 122],
      [12, 75, 47, 26, 76, 48],
      [39, 54, 24, 14, 55, 25],
      [22, 45, 15, 41, 46, 16],
      [6, 151, 121, 14, 152, 122],
      [6, 75, 47, 34, 76, 48],
      [46, 54, 24, 10, 55, 25],
      [2, 45, 15, 64, 46, 16],
      [17, 152, 122, 4, 153, 123],
      [29, 74, 46, 14, 75, 47],
      [49, 54, 24, 10, 55, 25],
      [24, 45, 15, 46, 46, 16],
      [4, 152, 122, 18, 153, 123],
      [13, 74, 46, 32, 75, 47],
      [48, 54, 24, 14, 55, 25],
      [42, 45, 15, 32, 46, 16],
      [20, 147, 117, 4, 148, 118],
      [40, 75, 47, 7, 76, 48],
      [43, 54, 24, 22, 55, 25],
      [10, 45, 15, 67, 46, 16],
      [19, 148, 118, 6, 149, 119],
      [18, 75, 47, 31, 76, 48],
      [34, 54, 24, 34, 55, 25],
      [20, 45, 15, 61, 46, 16]
    ];
    const qrRSBlock = function(totalCount, dataCount) {
      const _this2 = {};
      _this2.totalCount = totalCount;
      _this2.dataCount = dataCount;
      return _this2;
    };
    const _this = {};
    const getRsBlockTable = function(typeNumber, errorCorrectionLevel) {
      switch (errorCorrectionLevel) {
        case QRErrorCorrectionLevel.L:
          return RS_BLOCK_TABLE[(typeNumber - 1) * 4 + 0];
        case QRErrorCorrectionLevel.M:
          return RS_BLOCK_TABLE[(typeNumber - 1) * 4 + 1];
        case QRErrorCorrectionLevel.Q:
          return RS_BLOCK_TABLE[(typeNumber - 1) * 4 + 2];
        case QRErrorCorrectionLevel.H:
          return RS_BLOCK_TABLE[(typeNumber - 1) * 4 + 3];
        default:
          return;
      }
    };
    _this.getRSBlocks = function(typeNumber, errorCorrectionLevel) {
      const rsBlock = getRsBlockTable(typeNumber, errorCorrectionLevel);
      if (typeof rsBlock == "undefined") {
        throw "bad rs block @ typeNumber:" + typeNumber + "/errorCorrectionLevel:" + errorCorrectionLevel;
      }
      const length = rsBlock.length / 3;
      const list = [];
      for (let i = 0;i < length; i += 1) {
        const count = rsBlock[i * 3 + 0];
        const totalCount = rsBlock[i * 3 + 1];
        const dataCount = rsBlock[i * 3 + 2];
        for (let j = 0;j < count; j += 1) {
          list.push(qrRSBlock(totalCount, dataCount));
        }
      }
      return list;
    };
    return _this;
  }();
  var qrBitBuffer = function() {
    const _buffer = [];
    let _length = 0;
    const _this = {};
    _this.getBuffer = function() {
      return _buffer;
    };
    _this.getAt = function(index) {
      const bufIndex = Math.floor(index / 8);
      return (_buffer[bufIndex] >>> 7 - index % 8 & 1) == 1;
    };
    _this.put = function(num, length) {
      for (let i = 0;i < length; i += 1) {
        _this.putBit((num >>> length - i - 1 & 1) == 1);
      }
    };
    _this.getLengthInBits = function() {
      return _length;
    };
    _this.putBit = function(bit) {
      const bufIndex = Math.floor(_length / 8);
      if (_buffer.length <= bufIndex) {
        _buffer.push(0);
      }
      if (bit) {
        _buffer[bufIndex] |= 128 >>> _length % 8;
      }
      _length += 1;
    };
    return _this;
  };
  var qrNumber = function(data) {
    const _mode = QRMode.MODE_NUMBER;
    const _data = data;
    const _this = {};
    _this.getMode = function() {
      return _mode;
    };
    _this.getLength = function(buffer) {
      return _data.length;
    };
    _this.write = function(buffer) {
      const data2 = _data;
      let i = 0;
      while (i + 2 < data2.length) {
        buffer.put(strToNum(data2.substring(i, i + 3)), 10);
        i += 3;
      }
      if (i < data2.length) {
        if (data2.length - i == 1) {
          buffer.put(strToNum(data2.substring(i, i + 1)), 4);
        } else if (data2.length - i == 2) {
          buffer.put(strToNum(data2.substring(i, i + 2)), 7);
        }
      }
    };
    const strToNum = function(s) {
      let num = 0;
      for (let i = 0;i < s.length; i += 1) {
        num = num * 10 + chatToNum(s.charAt(i));
      }
      return num;
    };
    const chatToNum = function(c) {
      if ("0" <= c && c <= "9") {
        return c.charCodeAt(0) - 48;
      }
      throw "illegal char :" + c;
    };
    return _this;
  };
  var qrAlphaNum = function(data) {
    const _mode = QRMode.MODE_ALPHA_NUM;
    const _data = data;
    const _this = {};
    _this.getMode = function() {
      return _mode;
    };
    _this.getLength = function(buffer) {
      return _data.length;
    };
    _this.write = function(buffer) {
      const s = _data;
      let i = 0;
      while (i + 1 < s.length) {
        buffer.put(getCode(s.charAt(i)) * 45 + getCode(s.charAt(i + 1)), 11);
        i += 2;
      }
      if (i < s.length) {
        buffer.put(getCode(s.charAt(i)), 6);
      }
    };
    const getCode = function(c) {
      if ("0" <= c && c <= "9") {
        return c.charCodeAt(0) - 48;
      } else if ("A" <= c && c <= "Z") {
        return c.charCodeAt(0) - 65 + 10;
      } else {
        switch (c) {
          case " ":
            return 36;
          case "$":
            return 37;
          case "%":
            return 38;
          case "*":
            return 39;
          case "+":
            return 40;
          case "-":
            return 41;
          case ".":
            return 42;
          case "/":
            return 43;
          case ":":
            return 44;
          default:
            throw "illegal char :" + c;
        }
      }
    };
    return _this;
  };
  var qr8BitByte = function(data) {
    const _mode = QRMode.MODE_8BIT_BYTE;
    const _data = data;
    const _bytes = qrcode.stringToBytes(data);
    const _this = {};
    _this.getMode = function() {
      return _mode;
    };
    _this.getLength = function(buffer) {
      return _bytes.length;
    };
    _this.write = function(buffer) {
      for (let i = 0;i < _bytes.length; i += 1) {
        buffer.put(_bytes[i], 8);
      }
    };
    return _this;
  };
  var qrKanji = function(data) {
    const _mode = QRMode.MODE_KANJI;
    const _data = data;
    const stringToBytes = qrcode.stringToBytes;
    (function(c, code) {
      const test = stringToBytes(c);
      if (test.length != 2 || (test[0] << 8 | test[1]) != code) {
        throw "sjis not supported.";
      }
    })("友", 38726);
    const _bytes = stringToBytes(data);
    const _this = {};
    _this.getMode = function() {
      return _mode;
    };
    _this.getLength = function(buffer) {
      return ~~(_bytes.length / 2);
    };
    _this.write = function(buffer) {
      const data2 = _bytes;
      let i = 0;
      while (i + 1 < data2.length) {
        let c = (255 & data2[i]) << 8 | 255 & data2[i + 1];
        if (33088 <= c && c <= 40956) {
          c -= 33088;
        } else if (57408 <= c && c <= 60351) {
          c -= 49472;
        } else {
          throw "illegal char at " + (i + 1) + "/" + c;
        }
        c = (c >>> 8 & 255) * 192 + (c & 255);
        buffer.put(c, 13);
        i += 2;
      }
      if (i < data2.length) {
        throw "illegal char at " + (i + 1);
      }
    };
    return _this;
  };
  var byteArrayOutputStream = function() {
    const _bytes = [];
    const _this = {};
    _this.writeByte = function(b) {
      _bytes.push(b & 255);
    };
    _this.writeShort = function(i) {
      _this.writeByte(i);
      _this.writeByte(i >>> 8);
    };
    _this.writeBytes = function(b, off, len) {
      off = off || 0;
      len = len || b.length;
      for (let i = 0;i < len; i += 1) {
        _this.writeByte(b[i + off]);
      }
    };
    _this.writeString = function(s) {
      for (let i = 0;i < s.length; i += 1) {
        _this.writeByte(s.charCodeAt(i));
      }
    };
    _this.toByteArray = function() {
      return _bytes;
    };
    _this.toString = function() {
      let s = "";
      s += "[";
      for (let i = 0;i < _bytes.length; i += 1) {
        if (i > 0) {
          s += ",";
        }
        s += _bytes[i];
      }
      s += "]";
      return s;
    };
    return _this;
  };
  var base64EncodeOutputStream = function() {
    let _buffer = 0;
    let _buflen = 0;
    let _length = 0;
    let _base64 = "";
    const _this = {};
    const writeEncoded = function(b) {
      _base64 += String.fromCharCode(encode(b & 63));
    };
    const encode = function(n) {
      if (n < 0) {
        throw "n:" + n;
      } else if (n < 26) {
        return 65 + n;
      } else if (n < 52) {
        return 97 + (n - 26);
      } else if (n < 62) {
        return 48 + (n - 52);
      } else if (n == 62) {
        return 43;
      } else if (n == 63) {
        return 47;
      } else {
        throw "n:" + n;
      }
    };
    _this.writeByte = function(n) {
      _buffer = _buffer << 8 | n & 255;
      _buflen += 8;
      _length += 1;
      while (_buflen >= 6) {
        writeEncoded(_buffer >>> _buflen - 6);
        _buflen -= 6;
      }
    };
    _this.flush = function() {
      if (_buflen > 0) {
        writeEncoded(_buffer << 6 - _buflen);
        _buffer = 0;
        _buflen = 0;
      }
      if (_length % 3 != 0) {
        const padlen = 3 - _length % 3;
        for (let i = 0;i < padlen; i += 1) {
          _base64 += "=";
        }
      }
    };
    _this.toString = function() {
      return _base64;
    };
    return _this;
  };
  var base64DecodeInputStream = function(str) {
    const _str = str;
    let _pos = 0;
    let _buffer = 0;
    let _buflen = 0;
    const _this = {};
    _this.read = function() {
      while (_buflen < 8) {
        if (_pos >= _str.length) {
          if (_buflen == 0) {
            return -1;
          }
          throw "unexpected end of file./" + _buflen;
        }
        const c = _str.charAt(_pos);
        _pos += 1;
        if (c == "=") {
          _buflen = 0;
          return -1;
        } else if (c.match(/^\s$/)) {
          continue;
        }
        _buffer = _buffer << 6 | decode(c.charCodeAt(0));
        _buflen += 6;
      }
      const n = _buffer >>> _buflen - 8 & 255;
      _buflen -= 8;
      return n;
    };
    const decode = function(c) {
      if (65 <= c && c <= 90) {
        return c - 65;
      } else if (97 <= c && c <= 122) {
        return c - 97 + 26;
      } else if (48 <= c && c <= 57) {
        return c - 48 + 52;
      } else if (c == 43) {
        return 62;
      } else if (c == 47) {
        return 63;
      } else {
        throw "c:" + c;
      }
    };
    return _this;
  };
  var gifImage = function(width, height) {
    const _width = width;
    const _height = height;
    const _data = new Array(width * height);
    const _this = {};
    _this.setPixel = function(x, y, pixel) {
      _data[y * _width + x] = pixel;
    };
    _this.write = function(out) {
      out.writeString("GIF87a");
      out.writeShort(_width);
      out.writeShort(_height);
      out.writeByte(128);
      out.writeByte(0);
      out.writeByte(0);
      out.writeByte(0);
      out.writeByte(0);
      out.writeByte(0);
      out.writeByte(255);
      out.writeByte(255);
      out.writeByte(255);
      out.writeString(",");
      out.writeShort(0);
      out.writeShort(0);
      out.writeShort(_width);
      out.writeShort(_height);
      out.writeByte(0);
      const lzwMinCodeSize = 2;
      const raster = getLZWRaster(lzwMinCodeSize);
      out.writeByte(lzwMinCodeSize);
      let offset = 0;
      while (raster.length - offset > 255) {
        out.writeByte(255);
        out.writeBytes(raster, offset, 255);
        offset += 255;
      }
      out.writeByte(raster.length - offset);
      out.writeBytes(raster, offset, raster.length - offset);
      out.writeByte(0);
      out.writeString(";");
    };
    const bitOutputStream = function(out) {
      const _out = out;
      let _bitLength = 0;
      let _bitBuffer = 0;
      const _this2 = {};
      _this2.write = function(data, length) {
        if (data >>> length != 0) {
          throw "length over";
        }
        while (_bitLength + length >= 8) {
          _out.writeByte(255 & (data << _bitLength | _bitBuffer));
          length -= 8 - _bitLength;
          data >>>= 8 - _bitLength;
          _bitBuffer = 0;
          _bitLength = 0;
        }
        _bitBuffer = data << _bitLength | _bitBuffer;
        _bitLength = _bitLength + length;
      };
      _this2.flush = function() {
        if (_bitLength > 0) {
          _out.writeByte(_bitBuffer);
        }
      };
      return _this2;
    };
    const getLZWRaster = function(lzwMinCodeSize) {
      const clearCode = 1 << lzwMinCodeSize;
      const endCode = (1 << lzwMinCodeSize) + 1;
      let bitLength = lzwMinCodeSize + 1;
      const table = lzwTable();
      for (let i = 0;i < clearCode; i += 1) {
        table.add(String.fromCharCode(i));
      }
      table.add(String.fromCharCode(clearCode));
      table.add(String.fromCharCode(endCode));
      const byteOut = byteArrayOutputStream();
      const bitOut = bitOutputStream(byteOut);
      bitOut.write(clearCode, bitLength);
      let dataIndex = 0;
      let s = String.fromCharCode(_data[dataIndex]);
      dataIndex += 1;
      while (dataIndex < _data.length) {
        const c = String.fromCharCode(_data[dataIndex]);
        dataIndex += 1;
        if (table.contains(s + c)) {
          s = s + c;
        } else {
          bitOut.write(table.indexOf(s), bitLength);
          if (table.size() < 4095) {
            if (table.size() == 1 << bitLength) {
              bitLength += 1;
            }
            table.add(s + c);
          }
          s = c;
        }
      }
      bitOut.write(table.indexOf(s), bitLength);
      bitOut.write(endCode, bitLength);
      bitOut.flush();
      return byteOut.toByteArray();
    };
    const lzwTable = function() {
      const _map = {};
      let _size = 0;
      const _this2 = {};
      _this2.add = function(key) {
        if (_this2.contains(key)) {
          throw "dup key:" + key;
        }
        _map[key] = _size;
        _size += 1;
      };
      _this2.size = function() {
        return _size;
      };
      _this2.indexOf = function(key) {
        return _map[key];
      };
      _this2.contains = function(key) {
        return typeof _map[key] != "undefined";
      };
      return _this2;
    };
    return _this;
  };
  var createDataURL = function(width, height, getPixel) {
    const gif = gifImage(width, height);
    for (let y = 0;y < height; y += 1) {
      for (let x = 0;x < width; x += 1) {
        gif.setPixel(x, y, getPixel(x, y));
      }
    }
    const b = byteArrayOutputStream();
    gif.write(b);
    const base64 = base64EncodeOutputStream();
    const bytes = b.toByteArray();
    for (let i = 0;i < bytes.length; i += 1) {
      base64.writeByte(bytes[i]);
    }
    base64.flush();
    return "data:image/gif;base64," + base64;
  };
  var qrcode_default = qrcode;
  var stringToBytes = qrcode.stringToBytes;

  // src/deck/qr.ts
  var SVG_NS = "http://www.w3.org/2000/svg";
  function qrSvg(text, label) {
    const qr = qrcode_default(0, "M");
    qr.addData(text);
    qr.make();
    const n = qr.getModuleCount();
    const margin = 3;
    const size = n + margin * 2;
    let d = "";
    for (let row = 0;row < n; row++) {
      for (let col = 0;col < n; col++) {
        if (qr.isDark(row, col))
          d += `M${col + margin} ${row + margin}h1v1h-1z`;
      }
    }
    const svg = document.createElementNS(SVG_NS, "svg");
    svg.setAttribute("viewBox", `0 0 ${size} ${size}`);
    svg.setAttribute("shape-rendering", "crispEdges");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", label);
    svg.dataset.lpText = text;
    const bg = document.createElementNS(SVG_NS, "rect");
    bg.setAttribute("width", String(size));
    bg.setAttribute("height", String(size));
    bg.setAttribute("fill", "#fff");
    const path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("d", d);
    path.setAttribute("fill", "#111");
    svg.append(bg, path);
    return svg;
  }

  // src/deck/overlay.ts
  class QrOverlay {
    el;
    constructor(parent, url, short) {
      this.el = h("div", { class: "lq-overlay", attrs: { role: "dialog", "aria-label": "Scan to join" } }, h("div", { class: "lq-overlay-card" }, h("div", { class: "lq-overlay-title", text: "Scan to join" }), h("div", { class: "lq-overlay-qr" }, qrSvg(url, `QR code for ${short}`)), h("div", { class: "lq-overlay-url", text: short })));
      this.el.hidden = true;
      this.el.addEventListener("click", () => this.hide());
      parent.append(this.el);
      document.addEventListener("keydown", (e) => {
        if (!this.el.hidden && e.key === "Escape") {
          e.preventDefault();
          e.stopImmediatePropagation();
          this.hide();
        }
      }, true);
    }
    get open() {
      return !this.el.hidden;
    }
    toggle() {
      if (this.open)
        this.hide();
      else
        this.el.hidden = false;
    }
    hide() {
      this.el.hidden = true;
    }
  }

  class Toast {
    el;
    timer;
    constructor(parent) {
      this.el = h("div", { class: "lq-toast", attrs: { role: "status", "aria-live": "polite" } });
      parent.append(this.el);
    }
    show(text, ms = 2600) {
      this.el.textContent = text;
      this.el.classList.add("lq-show");
      clearTimeout(this.timer);
      this.timer = setTimeout(() => this.el.classList.remove("lq-show"), ms);
    }
  }

  // src/deck/presenter-key.ts
  var storageKey = (server) => {
    let origin = server;
    try {
      origin = new URL(server).origin;
    } catch {}
    return `lq:key:${origin}`;
  };
  function storage() {
    try {
      return window.localStorage;
    } catch {
      return null;
    }
  }
  function takeKeyFromUrl(server, store) {
    const url = new URL(window.location.href);
    if (!url.searchParams.has("presenter"))
      return;
    const value = rawParam(url.search, "presenter");
    url.searchParams.delete("presenter");
    window.history.replaceState(window.history.state, "", url.toString());
    if (!server || !store)
      return;
    const s = storage();
    if (!s)
      return;
    try {
      if (value === "")
        s.removeItem(storageKey(server));
      else
        s.setItem(storageKey(server), value);
    } catch {}
  }
  function rawParam(search, name) {
    for (const part of search.replace(/^\?/, "").split("&")) {
      const eq = part.indexOf("=");
      const key = eq < 0 ? part : part.slice(0, eq);
      if (key !== name)
        continue;
      const value = eq < 0 ? "" : part.slice(eq + 1);
      try {
        return decodeURIComponent(value);
      } catch {
        return value;
      }
    }
    return "";
  }
  function storedKey(server) {
    try {
      return storage()?.getItem(storageKey(server)) ?? null;
    } catch {
      return null;
    }
  }

  // src/deck/bars.ts
  function labelNodes(li, fallback) {
    if (!li)
      return [document.createTextNode(fallback)];
    const only = li.children.length === 1 && li.firstElementChild?.tagName === "P" && li.textContent?.trim() === li.firstElementChild.textContent?.trim();
    const source = only ? li.firstElementChild : li;
    return Array.from(source.childNodes, (n) => n.cloneNode(true));
  }

  class BarChart {
    el;
    rows;
    constructor(options, items) {
      this.rows = options.map((text, i) => {
        const fill = h("div", { class: "lq-fill" });
        const pct = h("span", { class: "lq-pct", text: "0%" });
        const count = h("span", { class: "lq-n", text: "0" });
        const label = h("div", { class: "lq-label" });
        label.append(...labelNodes(items[i], text));
        const el = h("div", { class: "lq-row" }, label, h("div", { class: "lq-track" }, fill), h("div", { class: "lq-stat" }, count, pct));
        return { el, fill, pct, count };
      });
      this.el = h("div", { class: "lq-bars" }, ...this.rows.map((r) => r.el));
    }
    update(results, correct) {
      const counts = this.rows.map((_, i) => results?.counts[i] ?? 0);
      const total = counts.reduce((a, b) => a + b, 0);
      const max = Math.max(0, ...counts);
      this.rows.forEach((row, i) => {
        const n = counts[i] ?? 0;
        const share = total > 0 ? n / total : 0;
        row.fill.style.width = max > 0 ? `${(n / max * 100).toFixed(2)}%` : "0%";
        row.pct.textContent = `${Math.round(share * 100)}%`;
        row.count.textContent = String(n);
        row.el.classList.toggle("lq-lead", max > 0 && n === max);
        row.el.classList.toggle("lq-correct", !!correct && correct.includes(i));
        row.el.classList.toggle("lq-dim", !!correct && !correct.includes(i));
      });
      this.el.classList.toggle("lq-revealed", !!correct);
    }
  }

  // src/deck/likert.ts
  var STEPS = { 1: [3], 2: [2, 4], 3: [1, 3, 5], 4: [1, 2, 4, 5], 5: [1, 2, 3, 4, 5] };
  function tones(n) {
    const arm = Math.floor(n / 2);
    const steps = STEPS[arm] ?? STEPS[5];
    return Array.from({ length: n }, (_, i) => {
      if (n % 2 === 1 && i === arm)
        return { side: "mid", step: 0 };
      const away = i < arm ? arm - i : i - (n - arm) + 1;
      return { side: i < arm ? "neg" : "pos", step: steps[away - 1] };
    });
  }
  function diverging(counts) {
    const n = counts.length;
    const total = counts.reduce((a, b) => a + b, 0);
    const share = counts.map((c) => total > 0 ? c / total : 0);
    const arm = Math.floor(n / 2);
    const neutral = n % 2 === 1 ? share[arm] : 0;
    const neg = share.slice(0, arm).reduce((a, b) => a + b, 0);
    const pos = share.slice(n - arm).reduce((a, b) => a + b, 0);
    let at = 0 - (neg + neutral / 2);
    const start = share.map((s) => {
      const x = at;
      at += s;
      return x;
    });
    return { start, width: share, neg, pos, total };
  }
  function extent(rows) {
    let left = 0;
    let right = 0;
    for (const r of rows) {
      const last = r.start.length - 1;
      left = Math.max(left, -(r.start[0] ?? 0));
      right = Math.max(right, (r.start[last] ?? 0) + (r.width[last] ?? 0));
    }
    const round = (x) => Math.min(1, Math.max(0.2, Math.ceil(x * 10 - 0.000000001) / 10));
    return { left: round(left), right: round(right) };
  }
  function summary(counts, values) {
    const total = counts.reduce((a, b) => a + b, 0);
    if (total === 0)
      return null;
    let sum = 0;
    counts.forEach((c, i) => sum += c * values[i]);
    const nth = (k) => {
      let seen = 0;
      for (let i = 0;i < counts.length; i++) {
        seen += counts[i];
        if (seen > k)
          return values[i];
      }
      return values[values.length - 1];
    };
    const median = (nth(Math.floor((total - 1) / 2)) + nth(Math.ceil((total - 1) / 2))) / 2;
    return { mean: sum / total, median, total };
  }
  var pctText = (x) => `${Math.round(x * 100)}%`;
  var responses = (n) => `${n} ${n === 1 ? "response" : "responses"}`;

  class LikertChart {
    q;
    el;
    diverging = [];
    axes = [];
    hist = [];
    values;
    constructor(q, items) {
      this.q = q;
      const n = q.labels.length;
      this.values = q.labels.map(Number);
      const showLabels = !(q.statements.length === 1 && q.statements[0] === q.prompt);
      const label = (i) => {
        const el = h("div", { class: "lq-lk-label" });
        el.append(...labelNodes(items[i], q.statements[i] ?? ""));
        return showLabels ? el : null;
      };
      const rows = h("div", { class: "lq-lk-rows" });
      if (!q.numeric) {
        const palette = tones(n);
        for (let s = 0;s < q.statements.length; s++) {
          const segs = [];
          const pcts = [];
          for (let p = 0;p < n; p++) {
            const pct = h("span", { class: "lq-lk-pct" });
            const seg = h("div", {
              class: "lq-lk-seg",
              attrs: { "data-side": palette[p].side, "data-step": String(palette[p].step), title: q.labels[p] }
            }, pct);
            segs.push(seg);
            pcts.push(pct);
          }
          const neg = h("span", { class: "lq-lk-total lq-lk-neg" });
          const pos = h("span", { class: "lq-lk-total lq-lk-pos" });
          const count = h("span", { class: "lq-lk-n" });
          const axis = h("div", { class: "lq-lk-axis" });
          this.axes.push(axis);
          rows.append(h("div", { class: "lq-lk-row" }, h("div", { class: "lq-lk-head" }, label(s), count), h("div", { class: "lq-lk-line" }, neg, h("div", { class: "lq-lk-track" }, axis, ...segs), pos)));
          this.diverging.push({ segs, pcts, neg, pos, n: count });
        }
      } else {
        for (let s = 0;s < q.statements.length; s++) {
          const bars = [];
          const counts = [];
          const cols = q.labels.map(() => {
            const c = h("span", { class: "lq-hist-n" });
            const bar = h("div", { class: "lq-hist-bar" }, c);
            bars.push(bar);
            counts.push(c);
            return h("div", { class: "lq-hist-col" }, bar);
          });
          const mean = h("div", { class: "lq-hist-mean", attrs: { "aria-hidden": "true" } });
          const axis = h("div", { class: "lq-hist-axis" }, ...q.labels.map((l) => h("span", { text: l })));
          const stats = h("span", { class: "lq-lk-stats" });
          const count = h("span", { class: "lq-lk-n" });
          rows.append(h("div", { class: "lq-lk-row" }, h("div", { class: "lq-lk-head" }, label(s), h("span", { class: "lq-lk-meta" }, stats, count)), h("div", { class: "lq-hist" }, ...cols, mean), axis));
          this.hist.push({ bars, counts, mean, stats, n: count });
        }
      }
      this.el = h("div", {
        class: `lq-likert ${q.numeric ? "lq-lk-numeric" : "lq-lk-labeled"}${n <= 3 ? " lq-lk-short" : ""}`,
        attrs: { "data-statements": String(q.statements.length) }
      }, this.legend(), rows);
      this.el.style.setProperty("--lq-points", String(n));
    }
    legend() {
      const q = this.q;
      if (q.numeric) {
        const end = (value, text) => text ? h("span", { class: "lq-lk-anchor" }, h("b", { text: value }), ` ${text}`) : null;
        const anchored = !!(q.anchors?.[0] || q.anchors?.[1]);
        return h("div", { class: "lq-lk-legend lq-lk-anchors" }, end(q.labels[0], q.anchors?.[0]), anchored ? h("span", { class: "lq-lk-dash", attrs: { "aria-hidden": "true" } }) : null, end(q.labels[q.labels.length - 1], q.anchors?.[1]), h("span", { class: "lq-lk-key lq-lk-mean-key" }, h("i", { class: "lq-lk-mean-icon" }), "mean"));
      }
      const palette = tones(q.labels.length);
      return h("div", { class: "lq-lk-legend" }, ...q.labels.map((l, i) => h("span", { class: "lq-lk-key" }, h("i", { class: "lq-lk-swatch", attrs: { "data-side": palette[i].side, "data-step": String(palette[i].step) } }), l)));
    }
    update(results) {
      const counts = this.q.statements.map((_, s) => this.q.labels.map((__, p) => results?.counts[s]?.[p] ?? 0));
      if (!this.q.numeric)
        this.updateDiverging(counts);
      else
        this.updateHist(counts);
    }
    updateDiverging(counts) {
      const bars = counts.map((c) => diverging(c));
      const { left, right } = extent(bars);
      const span = left + right;
      const at = (x) => (x + left) / span * 100;
      for (const axis of this.axes)
        axis.style.left = `${at(0).toFixed(3)}%`;
      bars.forEach((b, s) => {
        const row = this.diverging[s];
        row.segs.forEach((seg, p) => {
          const w = b.width[p] ?? 0;
          seg.style.left = `${at(b.start[p] ?? 0).toFixed(3)}%`;
          seg.style.width = `${(w / span * 100).toFixed(3)}%`;
          seg.classList.toggle("lq-lk-zero", w === 0);
          row.pcts[p].textContent = w > 0 ? pctText(w) : "";
        });
        row.neg.textContent = b.total > 0 ? pctText(b.neg) : "";
        row.pos.textContent = b.total > 0 ? pctText(b.pos) : "";
        row.n.textContent = responses(b.total);
      });
    }
    updateHist(counts) {
      const max = Math.max(0, ...counts.flat());
      const n = this.q.labels.length;
      counts.forEach((c, s) => {
        const row = this.hist[s];
        c.forEach((count, p) => {
          row.bars[p].style.height = max > 0 ? `${(count / max * 100).toFixed(2)}%` : "0%";
          row.bars[p].classList.toggle("lq-hist-empty", count === 0);
          row.counts[p].textContent = count > 0 ? String(count) : "";
        });
        const sum = summary(c, this.values);
        row.n.textContent = responses(sum?.total ?? 0);
        row.stats.textContent = sum ? `mean ${sum.mean.toFixed(1)} · median ${sum.median}` : "";
        row.mean.hidden = !sum;
        if (sum) {
          const x = (sum.mean - this.values[0] + 0.5) / n * 100;
          row.mean.style.left = `${x.toFixed(3)}%`;
        }
      });
    }
  }

  // src/deck/cloud-layout.ts
  function newMemory() {
    return { positions: new Map, scale: 1 };
  }
  function hash(s) {
    let h2 = 2166136261;
    for (let i = 0;i < s.length; i++) {
      h2 ^= s.charCodeAt(i);
      h2 = Math.imul(h2, 16777619);
    }
    return h2 >>> 0;
  }
  function fontSize(count, maxCount, minFont, maxFont) {
    const t = Math.sqrt(Math.max(0, count) / Math.max(1, maxCount));
    return minFont + (maxFont - minFont) * t;
  }
  function overlaps(a, b, gap) {
    return Math.abs(a.x - b.x) * 2 < a.w + b.w + gap * 2 && Math.abs(a.y - b.y) * 2 < a.h + b.h + gap * 2;
  }
  function inside(b, width, height) {
    return Math.abs(b.x) * 2 + b.w <= width && Math.abs(b.y) * 2 + b.h <= height;
  }
  function fits(b, placed, o) {
    if (!inside(b, o.width, o.height))
      return false;
    for (const p of placed)
      if (overlaps(b, p, o.gap))
        return false;
    return true;
  }
  function search(w, h2, x0, y0, phase, placed, o) {
    const aspect = o.width / o.height;
    const maxR = Math.hypot(o.width, o.height);
    const step = 0.12;
    for (let t = 0;; t += step) {
      const r = 1.6 * t;
      if (r / Math.max(1, aspect) > maxR)
        return null;
      const x = x0 + r * Math.cos(t + phase) * aspect;
      const y = y0 + r * Math.sin(t + phase);
      if (fits({ x, y, w, h: h2 }, placed, o))
        return { x, y };
    }
  }
  function attempt(words, memory, scale, o) {
    const maxCount = Math.max(1, ...words.map((w) => w.count));
    const sized = words.map((w) => {
      const size = fontSize(w.count, maxCount, o.minFont, o.maxFont) * scale;
      return { ...w, size, ...o.measure(w.text, size) };
    });
    const placed = [];
    const pending = [];
    for (const word of sized) {
      const at = memory.get(word.key);
      if (!at) {
        pending.push({ word, from: null });
        continue;
      }
      const box = { x: at.x, y: at.y, w: word.w, h: word.h };
      if (fits(box, placed, o))
        placed.push({ ...word, x: at.x, y: at.y });
      else
        pending.push({ word, from: at });
    }
    for (const { word, from } of pending) {
      const phase = hash(word.key) % 628 / 100;
      const spot = search(word.w, word.h, from?.x ?? 0, from?.y ?? 0, phase, placed, o) ?? (from ? search(word.w, word.h, 0, 0, phase, placed, o) : null);
      if (!spot)
        return null;
      placed.push({ ...word, ...spot });
    }
    return { placed, moved: pending.filter((p) => p.from).length };
  }
  var SHRINK = 0.88;
  var MIN_SCALE = 0.6;
  function layoutCloud(words, memory, o) {
    const accept = (r, scale) => {
      memory.scale = scale;
      const keep = new Set(r.placed.map((p) => p.key));
      for (const key of memory.positions.keys())
        if (!keep.has(key))
          memory.positions.delete(key);
      for (const p of r.placed)
        memory.positions.set(p.key, { x: p.x, y: p.y });
      return r.placed;
    };
    if (words.length === 0) {
      memory.positions.clear();
      memory.scale = 1;
      return [];
    }
    const up = Math.min(1, memory.scale / SHRINK);
    if (up > memory.scale) {
      const r = attempt(words, memory.positions, up, o);
      if (r && r.moved === 0)
        return accept(r, up);
    }
    let list = words;
    while (list.length > 0) {
      for (let scale = memory.scale;scale >= MIN_SCALE; scale *= SHRINK) {
        const r = attempt(list, memory.positions, scale, o);
        if (r)
          return accept(r, scale);
      }
      list = list.slice(0, Math.floor(list.length * 0.8));
    }
    return [];
  }

  // src/deck/cloud.ts
  var MAX_WORDS = 70;

  class WordCloud {
    onHide;
    el;
    area;
    tray;
    spans = new Map;
    memory = newMemory();
    canvas = null;
    words = [];
    hidden = [];
    moderated = false;
    constructor(onHide) {
      this.onHide = onHide;
      this.area = h("div", { class: "lq-cloud-area" });
      this.tray = h("div", { class: "lq-tray" });
      this.tray.hidden = true;
      this.el = h("div", { class: "lq-cloud" }, this.area, this.tray);
    }
    setModerated(on) {
      if (on === this.moderated)
        return;
      this.moderated = on;
      this.el.classList.toggle("lq-moderated", on);
      for (const span of this.spans.values())
        this.decorate(span);
      this.renderTray();
    }
    update(results) {
      this.words = results ? results.words.slice(0, MAX_WORDS) : [];
      this.hidden = results?.hidden ?? [];
      this.renderTray();
      this.layout();
    }
    layout() {
      const width = this.area.clientWidth;
      const height = this.area.clientHeight;
      if (width === 0 || height === 0)
        return;
      const style = getComputedStyle(this.area);
      const family = style.fontFamily;
      const weight = style.fontWeight;
      const measure = (text, size) => {
        const ctx = this.context();
        if (!ctx)
          return { w: text.length * size * 0.6, h: size * 1.2 };
        ctx.font = `${weight} ${size}px ${family}`;
        return { w: ctx.measureText(text).width + size * 0.2, h: size * 1.12 };
      };
      const maxFont = Math.min(96, height / 3.2);
      const placed = layoutCloud(this.words.map((w) => ({ key: w.text.toLowerCase(), text: w.text, count: w.count })), this.memory, { width, height, minFont: Math.min(22, maxFont / 2), maxFont, gap: 5, measure });
      const seen = new Set;
      placed.forEach((p, rank) => {
        seen.add(p.key);
        let span = this.spans.get(p.key);
        const isNew = !span;
        if (!span) {
          span = this.createSpan();
          this.spans.set(p.key, span);
          this.area.append(span);
        }
        span.textContent = p.text;
        span.dataset.text = p.text;
        span.dataset.count = String(p.count);
        this.decorate(span);
        const tone = rank < 3 ? 0 : p.count <= 1 && placed.length > 12 ? 4 : 1 + hash(p.key) % 3;
        span.dataset.tone = String(tone);
        span.style.fontSize = `${p.size.toFixed(1)}px`;
        span.style.transform = `translate(-50%, -50%) translate(${p.x.toFixed(1)}px, ${p.y.toFixed(1)}px)`;
        if (isNew) {
          span.classList.add("lq-enter");
          requestAnimationFrame(() => requestAnimationFrame(() => span.classList.remove("lq-enter")));
        }
      });
      for (const [key, span] of this.spans) {
        if (seen.has(key))
          continue;
        span.remove();
        this.spans.delete(key);
      }
      this.el.classList.toggle("lq-empty", placed.length === 0);
    }
    createSpan() {
      const span = h("span", { class: "lq-word" });
      const hide = (e) => {
        if (!this.moderated)
          return;
        e.preventDefault();
        e.stopPropagation();
        this.onHide(span.dataset.text ?? "", true);
      };
      span.addEventListener("click", hide);
      span.addEventListener("keydown", (e) => {
        if (e.key === "Enter" || e.key === " ")
          hide(e);
      });
      return span;
    }
    decorate(span) {
      const text = span.dataset.text ?? "";
      const tip = `${span.dataset.count ?? "?"} × “${text}”`;
      if (this.moderated) {
        span.tabIndex = 0;
        span.setAttribute("role", "button");
        span.title = `${tip} (click to hide)`;
      } else {
        span.removeAttribute("tabindex");
        span.removeAttribute("role");
        span.title = tip;
      }
    }
    renderTray() {
      if (!this.moderated || this.hidden.length === 0) {
        this.tray.replaceChildren();
        this.tray.hidden = true;
        return;
      }
      this.tray.hidden = false;
      this.tray.replaceChildren(h("span", { class: "lq-tray-label", text: "Hidden" }), ...this.hidden.map((text) => {
        const b = button(text, () => this.onHide(text, false), "lq-chip");
        b.title = `Show “${text}” again`;
        return b;
      }));
    }
    context() {
      if (!this.canvas)
        this.canvas = document.createElement("canvas").getContext("2d");
      return this.canvas;
    }
  }

  // src/deck/question.ts
  var PILL_TEXT = { live: "LIVE", closed: "CLOSED", offline: "OFFLINE" };

  class QuestionView {
    el;
    q;
    ctx;
    chart;
    main;
    pill;
    count;
    countLabel;
    joined;
    controls;
    toggleBtn;
    presenter = false;
    resultsOverride = null;
    last = { question: null, live: null, link: "connecting", audience: 0 };
    constructor(el, q, ctx) {
      this.el = el;
      this.q = q;
      this.ctx = ctx;
      el.classList.add("lq-active");
      const text = el.querySelector(":scope > .lq-text");
      const list = el.querySelector(":scope > ul, :scope > ol");
      list?.classList.add("lq-source");
      if (q.type === "poll") {
        this.chart = new BarChart(q.options, list ? Array.from(list.children) : []);
      } else if (q.type === "likert") {
        this.chart = new LikertChart(q, list ? Array.from(list.children) : []);
      } else {
        this.chart = new WordCloud((t, hidden) => ctx.send({ t: "hide", id: q.id, text: t, hidden }));
      }
      const veil = h("div", { class: "lq-veil", text: "Results hidden" });
      if (!(this.chart instanceof WordCloud))
        this.chart.el.append(veil);
      this.main = h("div", { class: "lq-main" }, text, this.chart.el, this.chart instanceof WordCloud ? veil : null);
      this.pill = h("span", { class: "lq-pill", attrs: { "data-state": "offline" }, text: "OFFLINE" });
      this.count = h("span", { class: "lq-count-n", text: "0" });
      this.countLabel = h("span", { class: "lq-count-label", text: "responses" });
      this.joined = h("span", { class: "lq-joined" });
      this.toggleBtn = button("Close voting", () => this.toggleVoting());
      this.controls = h("div", { class: "lq-controls", attrs: { role: "toolbar", "aria-label": "Question controls" } }, this.toggleBtn, button("Reset", () => this.reset(), "lq-danger"));
      const side = h("div", { class: "lq-side" }, ctx.qr ? h("div", { class: "lq-qr" }, qrSvg(ctx.audienceUrl, `QR code for ${ctx.shortUrl}`)) : null, ctx.qr ? h("div", { class: "lq-url", text: ctx.shortUrl }) : null, h("div", { class: "lq-count" }, this.count, this.countLabel), h("div", { class: "lq-meta" }, this.pill, this.joined), this.controls);
      el.append(h("div", { class: "lq-body" }, this.main, side));
      this.applyResultsVisibility();
    }
    setPresenter(on) {
      this.presenter = on;
      this.el.classList.toggle("lq-presenter", on);
      if (this.chart instanceof WordCloud)
        this.chart.setModerated(on);
    }
    update(state) {
      this.last = state;
      const qs = state.question;
      const results = qs?.results ?? null;
      if (this.chart instanceof BarChart) {
        this.chart.update(results?.type === "poll" ? results : null, qs?.revealed ?? null);
      } else if (this.chart instanceof LikertChart) {
        this.chart.update(results?.type === "likert" ? results : null);
      } else {
        this.chart.update(results?.type === "wordcloud" ? results : null);
      }
      const voters = results?.voters ?? 0;
      this.count.textContent = String(voters);
      this.countLabel.textContent = voters === 1 ? "response" : "responses";
      this.joined.textContent = state.link === "online" ? `${state.audience} connected` : "";
      const pill = state.link !== "online" ? "offline" : state.live === this.q.id && qs?.status === "open" ? "live" : "closed";
      this.pill.dataset.state = pill;
      this.pill.textContent = PILL_TEXT[pill];
      this.el.dataset.status = pill;
      this.toggleBtn.textContent = pill === "live" ? "Close voting" : "Reopen voting";
      this.toggleBtn.disabled = pill === "offline";
      this.applyResultsVisibility();
    }
    toggleResults() {
      this.resultsOverride = !this.resultsVisible();
      this.applyResultsVisibility();
      this.refresh();
    }
    showResults() {
      this.resultsOverride = true;
      this.applyResultsVisibility();
    }
    refresh() {
      if (this.el.offsetParent === null)
        return;
      if (this.chart instanceof WordCloud)
        this.chart.layout();
      else
        this.fitBars();
    }
    fitBars() {
      const bars = this.chart.el;
      let scale = 1;
      bars.style.setProperty("--lq-fit", "1");
      while (scale > 0.6 && bars.scrollHeight > this.main.clientHeight - this.textHeight() + 1) {
        scale -= 0.05;
        bars.style.setProperty("--lq-fit", scale.toFixed(2));
      }
    }
    textHeight() {
      const text = this.main.querySelector(":scope > .lq-text");
      return text ? text.offsetHeight : 0;
    }
    resultsVisible() {
      if (this.resultsOverride !== null)
        return this.resultsOverride;
      return this.q.results === "live" || (this.last.question?.revealed ?? null) !== null;
    }
    applyResultsVisibility() {
      const visible = this.resultsVisible();
      this.el.classList.toggle("lq-results-hidden", !visible);
    }
    toggleVoting() {
      if (this.el.dataset.status === "live")
        this.ctx.send({ t: "close", id: this.q.id });
      else
        this.ctx.open(this.q);
    }
    reset() {
      const ok = window.confirm(`Clear all responses to “${this.q.prompt}”?`);
      if (ok)
        this.ctx.send({ t: "reset", id: this.q.id });
    }
  }

  // src/deck/live-questions.ts
  var KEY_Q = 81;
  var KEY_A = 65;
  var KEY_T = 84;
  function randomId() {
    if (typeof crypto.randomUUID === "function")
      return crypto.randomUUID();
    const b = crypto.getRandomValues(new Uint8Array(16));
    return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
  }
  function sameDefinition(a, b) {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  class LiveQuestions {
    deck;
    mode = "static";
    config;
    conn = null;
    views = [];
    state = null;
    link = "connecting";
    presenter = false;
    welcomes = 0;
    resyncPending = false;
    openedId = null;
    overlay = null;
    toast = null;
    key = null;
    constructor(deck) {
      this.deck = deck;
    }
    init() {
      const config = readConfig();
      const elements = Array.from(document.querySelectorAll(".lq-question[data-lq]"));
      const embedded = isEmbedded();
      takeKeyFromUrl(config?.server, !embedded);
      const print = isPrint(window.location.search) || this.deck.isPrintView();
      if (!config || !config.server || print) {
        for (const el of elements)
          el.classList.add("lq-static");
        document.documentElement.dataset.lqMode = "static";
        return;
      }
      this.config = config;
      this.mode = embedded ? "embedded" : "viewer";
      document.documentElement.dataset.lqMode = this.mode;
      document.documentElement.dataset.lpTone = themeTone(this.deck.getRevealElement());
      const server = config.server;
      const session = sessionOverride(window.location.search) ?? config.session;
      const url = audienceUrl(server, audiencePath(session));
      const short = shortUrl(url);
      const reveal = this.deck.getRevealElement();
      this.toast = new Toast(reveal);
      this.overlay = new QrOverlay(reveal, url, short);
      const ctx = {
        qr: config.qr,
        audienceUrl: url,
        shortUrl: short,
        send: (msg) => this.send(msg),
        open: (q) => this.openQuestion(q, true)
      };
      const seen = new Set;
      for (const el of elements) {
        const q = readQuestion(el);
        if (!q)
          continue;
        if (seen.has(q.id))
          console.warn(`[live-questions] duplicate question id "${q.id}"`);
        seen.add(q.id);
        this.views.push(new QuestionView(el, q, ctx));
      }
      this.key = embedded ? null : storedKey(server);
      const voter = randomId();
      this.conn = new Connection(socketUrl(server, socketPath(session)), {
        hello: () => ({
          t: "hello",
          v: PROTOCOL_VERSION,
          role: "deck",
          voter,
          ...this.key ? { key: this.key } : {}
        }),
        message: (msg) => this.onMessage(msg),
        status: (status) => this.onLink(status)
      });
      this.registerKeys();
      this.deck.on("ready", () => this.onSlide());
      this.deck.on("slidechanged", () => this.onSlide());
      this.deck.on("resize", () => this.refreshCurrent());
      document.fonts?.ready.then(() => this.refreshCurrent());
      this.render();
      this.conn.start();
    }
    send(msg) {
      if (!this.presenter && msg.t !== "ping")
        return false;
      const sent = this.conn?.send(msg) ?? false;
      if (!sent)
        this.toast?.show("Not connected to the Live Questions server");
      return sent;
    }
    onLink(status) {
      this.link = status;
      if (status !== "online")
        this.setPresenter(false);
      this.render();
    }
    onMessage(msg) {
      switch (msg.t) {
        case "welcome": {
          this.welcomes++;
          this.setPresenter(msg.presenter);
          if (this.key && !msg.presenter && this.welcomes === 1) {
            this.toast?.show("Presenter key not accepted: viewing only", 5000);
          }
          if (msg.presenter) {
            if (this.welcomes === 1)
              this.syncSlide(true);
            else
              this.resyncPending = true;
          }
          break;
        }
        case "deck":
          this.state = msg.state;
          this.render();
          if (this.resyncPending) {
            this.resyncPending = false;
            this.syncSlide(false);
          }
          break;
        case "error":
          console.warn(`[live-questions] server error ${msg.code}: ${msg.message}`);
          if (this.presenter)
            this.toast?.show(`Live Questions: ${msg.message || msg.code}`);
          break;
        default:
          break;
      }
    }
    setPresenter(on) {
      this.presenter = on;
      const mode = on ? "presenter" : this.mode === "embedded" ? "embedded" : "viewer";
      document.documentElement.dataset.lqMode = mode;
      for (const v of this.views)
        v.setPresenter(on);
    }
    currentView() {
      const slide = this.deck.getCurrentSlide();
      if (!slide)
        return null;
      return this.views.find((v) => slide.contains(v.el)) ?? null;
    }
    onSlide() {
      this.refreshCurrent();
      this.overlay?.hide();
      this.syncSlide(true);
    }
    syncSlide(force) {
      if (!this.presenter || this.link !== "online")
        return;
      const view = this.currentView();
      if (view) {
        this.openQuestion(view.q, force);
        return;
      }
      const id = this.openedId;
      if (!id || !this.config.closeOnLeave)
        return;
      const qs = this.state?.questions[id];
      if (!this.state || this.state.live === id && qs?.status === "open") {
        this.conn?.send({ t: "close", id });
      }
      this.openedId = null;
    }
    openQuestion(q, force) {
      const def = definition(q);
      const known = this.state?.questions[q.id];
      if (!force && this.state?.live === q.id && known && sameDefinition(known.question, def)) {
        this.openedId = q.id;
        return;
      }
      if (this.send({ t: "open", question: def }))
        this.openedId = q.id;
    }
    render() {
      for (const v of this.views) {
        v.update({
          question: this.state?.questions[v.q.id] ?? null,
          live: this.state?.live ?? null,
          link: this.link,
          audience: this.state?.audience ?? 0
        });
      }
    }
    refreshCurrent() {
      const slide = this.deck.getCurrentSlide();
      if (!slide)
        return;
      const height = Number(this.deck.getConfig().height) || 700;
      for (const v of this.views) {
        if (!slide.contains(v.el))
          continue;
        fitHeight(v.el, slide, height);
        v.refresh();
      }
    }
    registerKeys() {
      this.deck.addKeyBinding({ keyCode: KEY_Q, key: "Q", description: "Live Questions: full-screen QR code" }, () => this.overlay?.toggle());
      this.deck.addKeyBinding({ keyCode: KEY_A, key: "A", description: "Live Questions: reveal the correct answer" }, () => this.revealAnswer());
      this.deck.addKeyBinding({ keyCode: KEY_T, key: "T", description: "Live Questions: show or hide results" }, () => {
        const view = this.currentView();
        if (view)
          view.toggleResults();
        else
          this.toast?.show("No live question on this slide");
      });
    }
    revealAnswer() {
      const view = this.currentView();
      if (!view)
        return this.toast?.show("No live question on this slide");
      if (!this.presenter)
        return this.toast?.show("Only the presenter can reveal answers");
      const q = view.q;
      if (q.type === "likert")
        return this.toast?.show("Rating scales have no answer to reveal");
      if (q.type !== "poll" || !q.correct?.length) {
        return this.toast?.show("This question has no correct answer marked");
      }
      if (this.send({ t: "reveal", id: q.id, correct: q.correct }))
        view.showResults();
    }
  }
  function themeTone(reveal) {
    const probe = document.createElement("span");
    probe.style.color = "var(--r-background-color, #fff)";
    reveal.append(probe);
    const rgb = getComputedStyle(probe).color.match(/[\d.]+/g)?.map(Number) ?? [255, 255, 255];
    probe.remove();
    const [r = 255, g = 255, b = 255] = rgb;
    return 0.2126 * r + 0.7152 * g + 0.0722 * b < 128 ? "dark" : "light";
  }
  function fitHeight(el, slide, slideHeight) {
    let top = 0;
    let node = el;
    while (node && node !== slide) {
      top += node.offsetTop;
      const parent = node.offsetParent;
      if (!parent || !slide.contains(parent) && parent !== slide)
        break;
      node = parent;
    }
    const available = slideHeight - top - 24;
    el.style.height = `${Math.round(Math.max(280, Math.min(620, available)))}px`;
  }

  // src/deck/plugin.ts
  function RevealLiveQuestions() {
    return {
      id: "RevealLiveQuestions",
      init(deck) {
        try {
          new LiveQuestions(deck).init();
        } catch (err) {
          console.error("[live-questions] failed to start", err);
        }
      }
    };
  }
  window.RevealLiveQuestions = RevealLiveQuestions;
})();
