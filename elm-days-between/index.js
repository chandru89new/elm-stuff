import { Elm } from "./src/Main.elm";
import anyDateParser from 'any-date-parser'
import { differenceInCalendarDays, differenceInCalendarMonths, differenceInCalendarYears, formatDistance, formatDistanceStrict } from 'date-fns'

let app = Elm.Main.init({ node: document.getElementById("app") });

function ParseError(message) {
  this.name = "ParseError"
  this.message = message
}

ParseError.prototype = Error.prototype

app.ports.sendDatesToDateFns.subscribe(({ startDate, endDate }) => {
  try {
    let [s, e] = [startDate, endDate].map((date) => anyDateParser.fromString(date))
    let data = formatDistance(e, s, { roundingMethod: "floor" })
    app.ports.getDaysBetweenFromDateFns.send({
      data,
      error: ''
    })
  } catch (e) {
    app.ports.getDaysBetweenFromDateFns.send({
      data: "",
      error: e.toString()
    })
  }

})

