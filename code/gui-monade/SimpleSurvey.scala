package de.ag.scala

import de.ag.survey.gui.SurveyGUI._
import de.ag.survey.dsl.SurveyMonad._
import de.ag.survey.dsl.UI._
import de.ag.survey.dsl.UI.Constraint._
import de.ag.survey.dsl._
import de.ag.survey.dsl.UI.ProgressBar
import de.ag.survey.dsl.UI.Text
import de.ag.survey.dsl.UI
import de.ag.survey.dsl.ChosenOne
import de.ag.survey.dsl.ChooseOneQ
import de.ag.survey.dsl.UI.Button

object SimpleSurvey extends App {
  case object Ja extends AnswerChoice(0)
  case object Nein extends AnswerChoice(1)

  case object Morgens extends AnswerChoice(2)
  case object Nachmittags extends AnswerChoice(3)
  case object Abends extends AnswerChoice(4)

  val montagZeitQ = ChooseOneQ(1, Set(Ja, Nein))
  val uhrzeitQ = EnterTextQ(3)

  val seite1 =
    put(1, 1, Text("Haben Sie am Montag Zeit?")) >>
    put(1, 2, Button("Ja", addAnswer(montagZeitQ, ChosenOne(Ja)))) >>
    put(1, 3, Button("Nein", addAnswer(montagZeitQ, ChosenOne(Nein))))

  val seite2 =
    put(1, 1, Text("Um wieviel Uhr?")) >>
    put(1, 2, TextField(onValueChanged=Some({t => addAnswer(uhrzeitQ, EnteredText(t)) >> ask})),
      fill=Fill.Horizontal, weightX=1.0) >>
    put(1, 3, Button("Weiter", doNothing))

  val letzteSeite =
    put(1, 1, Text("Vielen Dank für die Teilnahme!")) >>
    put(2, 1, Button("OK", doNothing))

  def progressBar(at:Int) =
    put(2, 1, ProgressBar(1, at, 3),
      width=3, fill=Fill.Horizontal, weightX=1.0,
      insets=Insets(top=10, left=20, bottom=10, right=20))

  val printResults =
    getAnswers >>= {answers =>
      answers(montagZeitQ) match {
        case ChosenOne(Ja) =>
          val EnteredText(uhrzeit) = answers(uhrzeitQ)
          println("Hat am Montag um %s Uhr Zeit.".format(uhrzeit))
        case ChosenOne(Nein) => println("Hat am Montag keine Zeit.")
      }
      doNothing
    }

  val kleineUmfrage =
    clear >>
    setTitle("Kleine Umfrage") >>
    seite1 >> ask >>
    (getAnswer(montagZeitQ) >>= {
      case Some(ChosenOne(Ja)) =>
        clear >> seite2 >> ask
      case Some(ChosenOne(Nein)) => doNothing
    }) >>
    printResults >>
    clear >> letzteSeite >> ask >>=
    { _ => System.exit(0); doNothing}



  showSurvey(emptySurveyGUI(null, new UI.Dimension(400,150)), kleineUmfrage)
}
