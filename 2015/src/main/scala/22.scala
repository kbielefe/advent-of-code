package day22
import parse.{*, given}
import algorithms.{Cache, Memoize, Memoized}

type I = (Int, Int) ~ """Hit Points: (\d+)\nDamage: (\d+)"""

case class GameState(currentMana: Int, manaSpent: Int, myHp: Int, bossHp: Int, bossDamage: Int, shieldCounter: Int, poisonCounter: Int, rechargeCounter: Int, difficulty: Int):
  def applyDifficulty: GameState =
    copy(myHp = myHp - difficulty)

  def bossAttacks: GameState =
    val damageTaken = bossDamage - (if shieldCounter > 0 then 7 else 0)
    copy(myHp = myHp - damageTaken)

  def applyEffects: GameState =
    val newShieldCounter = math.max(0, shieldCounter - 1)
    val newPoisonCounter = math.max(0, poisonCounter - 1)
    val newRechargeCounter = math.max(0, rechargeCounter - 1)
    val newBossHp = if poisonCounter > 0 then bossHp - 3 else bossHp
    val newMana = if rechargeCounter > 0 then currentMana + 101 else currentMana
    copy(currentMana = newMana, bossHp = newBossHp, shieldCounter = newShieldCounter, poisonCounter = newPoisonCounter, rechargeCounter = newRechargeCounter)

  def magicMissile: GameState =
    copy(currentMana = currentMana - 53, manaSpent = manaSpent + 53, bossHp = bossHp - 4)

  def drain: GameState =
    copy(currentMana = currentMana - 73, manaSpent = manaSpent + 73, myHp = myHp + 2, bossHp = bossHp - 2)

  def shield: GameState =
    copy(currentMana = currentMana - 113, manaSpent = manaSpent + 113, shieldCounter = 6)

  def poison: GameState =
    copy(currentMana = currentMana - 173, manaSpent = manaSpent + 173, poisonCounter = 6)

  def recharge: GameState =
    copy(currentMana = currentMana - 229, manaSpent = manaSpent + 229, rechargeCounter = 5)

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    val initial = GameState(500, 0, 50, input._1, input._2, 0, 0, 0, 0)
    playMyTurn(initial)

  def part2(input: I): Int =
    val initial = GameState(500, 0, 50, input._1, input._2, 0, 0, 0, 1)
    playMyTurn(initial)

  given Cache[GameState, Int] = Cache.empty

  def playMyTurn(state: GameState): Int =
    val afterDifficulty = state.applyDifficulty
    val afterEffects = afterDifficulty.applyEffects
    if afterDifficulty.myHp <= 0 then
      Int.MaxValue
    else if afterEffects.bossHp <= 0 then
      afterEffects.manaSpent
    else if afterEffects.currentMana < 53 then
      Int.MaxValue
    else
      val choices = Set[GameState => GameState](_.magicMissile, _.drain, _.shield, _.poison, _.recharge)
        .map(_(afterEffects))
        .filter(_.currentMana > 0)
        .map(state => Memoize(state, playBossTurn(state)))

      if choices.isEmpty then
        Int.MaxValue
      else
        choices.min

  def playBossTurn(state: GameState): Memoized[GameState, Int] =
    val afterEffects = state.applyEffects
    if afterEffects.bossHp <= 0 then
      afterEffects.manaSpent
    else
      val afterAttack = afterEffects.bossAttacks
      if afterAttack.myHp <= 0 then
        Int.MaxValue
      else
        playMyTurn(afterAttack)
