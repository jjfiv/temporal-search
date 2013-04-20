package ciir.ts.gui
import java.awt.{Graphics, Color, Dimension}
import java.awt.image.BufferedImage
import javax.swing._

object ImageMaker {
  def basic(w: Int, h: Int) = {
    var img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    var gfx = img.getGraphics
    gfx.setColor(new Color(0,0,0xff))
    gfx.fillRect(0,0,w,h)
    img
  }
}

// http://stackoverflow.com/questions/299495/java-swing-how-to-add-an-image-to-a-jpanel
class ImagePanel(val img: BufferedImage) extends JPanel {
  val imageSize = new Dimension(img.getWidth, img.getHeight)
  setMinimumSize(imageSize)
  setMaximumSize(imageSize)
  setPreferredSize(imageSize)
  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}

object UIMain {
  def launch(args: Array[String]) {
    SwingUtilities.invokeLater(new Runnable { def run() { show() } })
  }
  def show() {
    var frame = new JFrame("Using Raw Swing")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val imageView = new ImagePanel(ImageMaker.basic(400,200))
    frame.add(imageView)
    frame.setMinimumSize(imageView.minimumSize)
    frame.pack()
    frame.setVisible(true)
  }
}

