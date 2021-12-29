package advent2020

object Day21:
  def part1(input: List[String]): Int =
    val foods = input.map(parseFood).toSet
    val allIngredients = foods.flatMap(_.ingredients)
    val potentialAllergens = foods.foldLeft(Map.empty[String, Set[String]]){case (acc, food) =>
      food.allergens.foldLeft(acc){case (acc, allergen) =>
        val newIngredients = acc.get(allergen).map(ingredients => food.ingredients & ingredients).getOrElse(food.ingredients)
        acc + (allergen -> newIngredients)
      }
    }
    val potentialAllergenIngredients = potentialAllergens.values.flatten.toSet
    val notAllergens = allIngredients -- potentialAllergenIngredients
    foods.toList.flatMap(_.ingredients).count(notAllergens contains _)

  def part2(input: List[String]): String =
    val foods = input.map(parseFood).toSet
    val allIngredients = foods.flatMap(_.ingredients)
    val potentialAllergens = foods.foldLeft(Map.empty[String, Set[String]]){case (acc, food) =>
      food.allergens.foldLeft(acc){case (acc, allergen) =>
        val newIngredients = acc.get(allergen).map(ingredients => food.ingredients & ingredients).getOrElse(food.ingredients)
        acc + (allergen -> newIngredients)
      }
    }
    uniqueAllergens(Map.empty, potentialAllergens).toList.sortBy(_._2).map(_._1).mkString(",")

  @scala.annotation.tailrec
  private def uniqueAllergens(acc: Map[String, String], potentialAllergens: Map[String, Set[String]]): Map[String, String] =
    if potentialAllergens.isEmpty then
      acc
    else
      val onlyOne = potentialAllergens.find(_._2.size == 1).get
      val allergen = onlyOne._1
      val ingredient = onlyOne._2.head
      val newPotentialAllergens = potentialAllergens.map((a, si) => (a -> (si - ingredient))).filterNot(_._2.isEmpty).toMap
      uniqueAllergens(acc + (ingredient -> allergen), newPotentialAllergens)

  private def parseFood(line: String): Food =
    val regex = """(.*)\(contains (.*)\)""".r
    line match
      case regex(ingredientString, allergenString) =>
        Food(ingredientString.split(" ").toSet, allergenString.split(", ").toSet)

  case class Food(ingredients: Set[String], allergens: Set[String])
