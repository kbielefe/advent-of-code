package algorithms

import _root_.breeze.linalg.{DenseMatrix, Matrix}

object Transforms:
  def translate(x: Int, y: Int): Matrix[Int] =
    DenseMatrix(
      (1, 0, x),
      (0, 1, y),
      (0, 0, 1)
    )

  def rotateCCW: Matrix[Int] =
    DenseMatrix(
      (0, -1, 0),
      (1,  0, 0),
      (0,  0, 1)
    )

  def rotateCW: Matrix[Int] =
    DenseMatrix(
      ( 0, 1, 0),
      (-1, 0, 0),
      ( 0, 0, 1)
    )

  def reflectX: Matrix[Int] =
    DenseMatrix(
      (1,  0, 0),
      (0, -1, 0),
      (0,  0, 1)
    )

  def reflectY: Matrix[Int] =
    DenseMatrix(
      (-1, 0, 0),
      ( 0, 1, 0),
      ( 0, 0, 1)
    )
