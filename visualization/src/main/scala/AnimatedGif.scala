package visualization
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.metadata.IIOMetadataNode as INode
import javax.imageio.stream.FileImageOutputStream
import javax.imageio.{ImageIO, IIOImage, ImageTypeSpecifier}
import org.w3c.dom.Node

@main def main(): Unit =
  val loop = true
  val filename = "test.gif"
  val width = 200
  val height = 200
  val loopDelayMs = 2000
  val imageType = BufferedImage.TYPE_INT_ARGB

  val imageWriter = ImageIO.getImageWritersByFormatName("gif").next()
  val writeParam = imageWriter.getDefaultWriteParam()
  val imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(imageType)
  val imageMetadata = imageWriter.getDefaultImageMetadata(imageTypeSpecifier, writeParam)
  val formatName = imageMetadata.getNativeMetadataFormatName()
  val root = imageMetadata.getAsTree(formatName)

  val gce = root.getChild("GraphicControlExtension")
  gce.setAttribute("disposalMethod", "none")
  gce.setAttribute("userInputFlag", "FALSE")
  gce.setAttribute("transparentColorFlag", "FALSE")
  gce.setAttribute("delayTime", (loopDelayMs / 10).toString)

  val app = root.getChild("ApplicationExtensions").getChild("ApplicationExtension")
  app.setAttribute("applicationID", "NETSCAPE")
  app.setAttribute("authenticationCode", "2.0")
  app.setUserObject(Array[Byte](1, if loop then 0 else 1, 0))

  imageMetadata.setFromTree(formatName, root)
  val fos = new FileImageOutputStream(new File(filename))
  imageWriter.setOutput(fos)
  imageWriter.prepareWriteSequence(null)
  val bufferedImage = new BufferedImage(width, height, imageType)
  for
    x <- 50 to 150
    y <- 50 to 150
  do bufferedImage.setRGB(x, y, 0xffff0000)
  val image = new IIOImage(bufferedImage, null, imageMetadata)
  imageWriter.writeToSequence(image, writeParam)
  for
    x <- 50 to 150
    y <- 50 to 150
  do bufferedImage.setRGB(x, y, 0xff00ff00)
  val image2 = new IIOImage(bufferedImage, null, imageMetadata)
  imageWriter.writeToSequence(image2, writeParam)
  imageWriter.endWriteSequence()
  fos.close()
  imageWriter.dispose()

given [A]: CanEqual[A | Null, Null] = CanEqual.derived

extension (node: Node)
  def getChild(name: String): INode =
    children.find(_.getNodeName() == name).getOrElse{
      val child = new INode(name)
      node.appendChild(child)
      child
    }

  def children: Iterator[INode] =
    Iterator.unfold(node.getFirstChild().asInstanceOf[INode])(child =>
      if child == null then None else Some(child, child.getNextSibling().asInstanceOf[INode])
    )
