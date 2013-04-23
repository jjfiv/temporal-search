package ciir.ts.gui
import java.awt.{Graphics, Graphics2D, Color, Dimension}
import java.awt.image.BufferedImage
import javax.swing._

object ImageMaker {
  def basic(w: Int, h: Int) = {
    var img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    var gfx = img.createGraphics
    gfx.setColor(Color.WHITE)
    gfx.fillRect(0,0,w,h)
    gfx.dispose()
    img
  }
  def graph(data: Array[Int], w: Int, h: Int) = {
    val barWidth = w/data.size
    var img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    var gfx = img.createGraphics
    gfx.setColor(Color.WHITE)
    gfx.fillRect(0,0,w,h)

    gfx.setColor(Color.BLUE)
    data.zipWithIndex.foreach {
      case (0,_) => {}
      case (y,x) => {
        gfx.fillRect(x*barWidth,h-y,barWidth,y) //x,y,w,h
      }
    }

    gfx.dispose()
    img
  }
}

// http://stackoverflow.com/questions/299495/java-swing-how-to-add-an-image-to-a-jpanel
class ImagePanel(var img: BufferedImage) extends JPanel {
  setImage(img)
  def setImage(_img: BufferedImage) {
    img = _img
    val imageSize = new Dimension(img.getWidth, img.getHeight)
    setMinimumSize(imageSize)
    setMaximumSize(imageSize)
    setPreferredSize(imageSize)
    repaint()
  }
  override def paintComponent(g: Graphics) {
    super.paintComponent(g)
    g.drawImage(img, 0, 0, null)
  }
}

object UI {
  def runLater(op: =>Unit) {
    SwingUtilities.invokeLater(new Runnable { def run() { op } })
  }
}

object UIMain {
  def launch(args: Array[String]) {
    UI.runLater {
      show()
    }
  }
  def show() {
    var frame = new JFrame("Using Raw Swing")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val imageView = new ImagePanel(ImageMaker.basic(400,200))
    frame.add(imageView)
    frame.setMinimumSize(imageView.getMinimumSize)
    frame.pack()
    frame.setVisible(true)
  }
}

