**AFRICAST 2026 – Day 1: Time series data structures and graphics**

This is the Day 1 recording of AFRICAST 2026, a free online course in tidy time series analysis and forecasting with R. It was run by Forecasting for Social Good (F4SG) from 19 to 23 October 2026.

🎓 **Session 1: Basics of time series and data structures** (Mitchell O'Hara-Wild, Monash University)
After an introduction to the course and the team, this session covers what a forecast is and what data you need to make one. It then shows how to store time series in R as tidy `tsibble` objects:
• The three parts of a tsibble: index, keys and measured variables
• Setting the right temporal granularity (`yearquarter()`, `yearmonth()`, `yearweek()`…)
• A preview of `mixtime` for time zones, other calendars, cyclical time and durations
• Working with time series using dplyr: `filter()`, `select()`, `summarise()` and `mutate()`
• Worked examples: the Australian prison population and pharmaceutical (PBS) sales

📈 **Session 2: Time series patterns and basic graphics** (Abiodun Egwuenu, Charité – Universitätsmedizin Berlin)
How to spot the patterns in your data before you forecast:
• Level, trend, seasonality and cycles, including additive vs multiplicative seasonality
• Time plots, seasonal plots, seasonal subseries plots and calendar plots with `ggtime`
• Telling seasonal patterns apart from cyclic ones
• Lag plots, autocorrelation and reading ACF plots
• White noise

📚 **Materials**
• Course website (slides, exercises and recordings): https://workshop.f4sg.org/africast/
• Day 1 slides: https://workshop.f4sg.org/africast/sessions/day1/basics.html and https://workshop.f4sg.org/africast/sessions/day1/graphics.html
• Day 1 exercises: https://workshop.f4sg.org/africast/sessions/day1/exercises.html
• Get the exercises project in R: `usethis::use_course("https://workshop.f4sg.org/africast/exercises.zip")`
• Textbook: Forecasting: Principles and Practice (3rd ed.), free at https://otexts.com/fpp3/

🌍 **About AFRICAST**
AFRICAST is part of the Forecasting for Social Good (F4SG) initiative. It builds forecasting skills among researchers in Africa so that forecasts can inform planning and resource allocation. The project is led by Bahman Rostami-Tabar (Cardiff University). The instructors are Mitchell O'Hara-Wild, Abiodun Egwuenu, Harsha Halgamuwe Hewage and Mustafa Aslan, and Henry Kissinger Ochieng leads coordination. Learn more at https://www.f4sg.org/

#RStats #Forecasting #TimeSeries #F4SG #Africa #tidyverts
