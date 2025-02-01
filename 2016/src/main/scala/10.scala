package advent2016

object Day10:
  def part1(input: List[String]): Int =
    endState(input).bots.find(_._2.values == List(17, 61)).get._1

  def part2(input: List[String]): Int =
    endState(input).outputs.filter(_._1 < 3).map(_._2).product

  def endState(input: List[String]): State =
    input.foldLeft(State.empty){
      case (state, valRegex(value, bot)) =>
        state.giveValueToBot(value.toInt, bot.toInt)
      case (state, botRegex(bot, lowType, lowNum, highType, highNum)) =>
        state.giveActionToBot(bot.toInt, lowType, lowNum.toInt, highType, highNum.toInt)
    }

  val valRegex = """value (\d+) goes to bot (\d+)""".r
  val botRegex = """bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)""".r

  case class Bot(values: List[Int], action: State => State):
    def withValue(value: Int): Bot =
      Bot((value :: values).sorted, action)

    def withAction(botNumber: Int, lowType: String, lowNum: Int, highType: String, highNum: Int): Bot =
      def lowAction(state: State): State =
        val value = state.bots(botNumber).values(0)
        if lowType == "output" then
          state.giveValueToOutput(value, lowNum)
        else
          state.giveValueToBot(value, lowNum)

      def highAction(state: State): State =
        val value = state.bots(botNumber).values(1)
        if highType == "output" then
          state.giveValueToOutput(value, highNum)
        else
          state.giveValueToBot(value, highNum)

      def action(state: State): State =
        val size = state.bots(botNumber).values.size
        if size == 2 then
          lowAction(highAction(state))
        else
          state

      Bot(values, action)

    end withAction

  case class State(bots: Map[Int, Bot], outputs: Map[Int, Int]):
    def giveValueToBot(value: Int, botNumber: Int): State =
      val bot = bots.getOrElse(botNumber, Bot.empty).withValue(value)
      bot.action(State(bots + (botNumber -> bot), outputs))

    def giveValueToOutput(value: Int, outputNumber: Int): State =
      State(bots, outputs + (outputNumber -> value))

    def giveActionToBot(botNumber: Int, lowType: String, lowNum: Int, highType: String, highNum: Int): State =
      val bot = bots.getOrElse(botNumber, Bot.empty).withAction(botNumber, lowType, lowNum, highType, highNum)
      bot.action(State(bots + (botNumber -> bot), outputs))

  object Bot:
    def empty = Bot(List.empty, identity)

  object State:
    def empty = State(Map.empty, Map.empty)
