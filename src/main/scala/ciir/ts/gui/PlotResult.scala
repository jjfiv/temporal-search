package ciir.ts.gui
import ciir.ts._
import javax.swing._
import java.awt.event._
import java.awt.{Dimension,Color}

class SearchPlotter(indexPath: String) {
  var retrieval = new DateRetrieval(indexPath)
  var queryField: JTextField = null
  var resultPane: JPanel = null
  var numResults = 0
  var results =  Seq[JPanel]()
  val GraphHeight = 100
  val GraphWidth = 400
  
  def search(query: String) {
    val NumYears = 100

    var data = new Array[Long](NumYears)
    retrieval.dateSearch(query).foreach {
      case DocDateScore(_, date, score) => data(date-1820) = score
    }
    
    //val range = data.max.toDouble
    val scaledData = data.map( tf => {
      tf.toInt
      //((tf.toDouble / range) * 400.0).toInt 
    })

    pushResultPanel(query, scaledData)
  }

  def debugBorder(comp: JComponent) {
    comp.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.RED), comp.getBorder))
  }

  def pushResultPanel(term: String, tf: Array[Int]) {
    var panel = new JPanel
    panel.add(new JLabel(term))
    panel.add(new ImagePanel(ImageMaker.graph(tf, GraphWidth, GraphHeight)))
    usePreferredSize(panel)
    //debugBorder(panel)
    
    numResults += 1
    results = results :+ panel

    resultPane.add(panel)
    resultPane.setPreferredSize(new Dimension(GraphWidth*2, (GraphHeight+20)*numResults))

    resultPane.revalidate()
  }

  def usePreferredSize(comp: java.awt.Component) {
    val pref = comp.getPreferredSize
    comp.setMaximumSize(pref)
    comp.setMinimumSize(pref)
  }
  
  def show() {
    queryField = new JTextField(20)
    queryField.addActionListener(new ActionListener {
      def actionPerformed(evt: ActionEvent) {
        val contents = queryField.getText()
        println("search: "+contents)
        search(contents)
      }
    })
    usePreferredSize(queryField)

    var frame = new JFrame("Graphing Queries")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    var contents = new JPanel
    contents.setLayout(new BoxLayout(contents, BoxLayout.PAGE_AXIS))

    contents.add(queryField)

    resultPane = new JPanel
    resultPane.setLayout(new BoxLayout(resultPane, BoxLayout.PAGE_AXIS))
    resultPane.setPreferredSize(new Dimension(2*GraphWidth,GraphHeight))
    var scrolledResults = new JScrollPane(resultPane)

    contents.add(scrolledResults)

    frame.add(contents)
    frame.pack()
    frame.setVisible(true)
  }
}

object PlotResult {
  def launch(args: Array[String]) {
    if(args.size != 1) {
      Util.quit("Expected arguments: indexDir")
    }
    val pv = new SearchPlotter(args(0))
    UI.runLater {
      pv.show()
    }
  }
}

