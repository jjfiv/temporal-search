package ciir.ts.gui
import ciir.ts._
import javax.swing._
import java.awt.{Dimension,Color}

class SearchPlotter(indexPath: String) {
  var retrieval = new DateRetrieval(indexPath)
  var resultPane: JPanel = null
  var numResults = 0
  val GraphHeight = 100
  val GraphWidth = 400

  def search(query: String) {
    clearResultPanel()

    var tf: Array[Long] = null

    // create a step function from given date
    if(query.startsWith("$") && query.tail.forall(_.isDigit) && query.tail.size==4 && retrieval.validDate(query.tail.toInt)) {
      tf = CurveFaker.stepFunction(retrieval, query.tail.toInt, 150)
    } else {
      // simple text search
      tf = retrieval.search(query)
    }

    assert(tf != null)

    val dates = retrieval.toDateVector(tf)
    pushResultPanel(query, dates.map(_.toInt))

    println("Finding similar...")
    retrieval.index.findSimilar(tf, 40) foreach {
      case SimilarTerm(term, score, data) => {
        val dates = retrieval.toDateVector(data).map(_.toInt)
        pushResultPanel("%s %.3f".format(term,score), dates)
      }
    }
    println("done...")
  }

  def clearResultPanel() {
    numResults = 0
    resultPane.removeAll()
    resultPane.revalidate()
  }

  def pushResultPanel(term: String, tf: Array[Int]) {
    var panel = new JPanel
    panel.add(new JLabel(term))
    panel.add(new ImagePanel(ImageMaker.graph(tf, GraphWidth, GraphHeight)))
    UI.usePreferredSize(panel)
    //UI.debugBorder(panel)
    
    numResults += 1

    resultPane.add(panel)
    resultPane.setPreferredSize(new Dimension(GraphWidth*2, (GraphHeight+20)*numResults))

    resultPane.revalidate()
  }

  def show() {
    var queryField = new JTextField(20)
    queryField.addActionListener(UI.actionResponder {
      val contents = queryField.getText()
      println("search: "+contents)
      search(contents)
    })
    UI.usePreferredSize(queryField)

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

