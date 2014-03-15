package application

/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 18.09.12
 * Time: 21:36
 * To change this template use File | Settings | File Templates.
 */

import swing._
import event.ButtonClicked
import matrix.Matrix
import scala.swing._
import java.awt.{Color, Desktop}
import java.io.{File, IOException}
import java.lang.Math
import javax.swing.{JFrame, JComponent, JFileChooser}


object View extends SimpleGUIApplication {
  var bb: Matrix = new Matrix(Array(Array(1.0)))
  var a: Matrix = new Matrix(Array(Array(1.0)))
  var aPath: String = ""
  var bPath: String  = ""
  def top = new MainFrame {
    val label = new Label {
      text = "Hello! I will show you the status of operations and mistakes"
    }
    label.foreground_=(Color.RED)

    // ...
    def rep(){this.repaint()}
    preferredSize = new Dimension(600, 700)
    title = "Subspace iterations of GE"
    minimumSize = new Dimension(500, 500)

    val buttonB = new Button {
      text = "Load B matrix"
    }
    val run = new Button {
      text = "Run"
    }
    val maxEigs = new TextField() {

    }
    val maxDim = new TextField() {

    }
    val but = new Button() {
      //size = new Dimension(50, 50)
      text = "Close"
    }
    val buttonA = new Button {
      text = "Load A matrix"

    }
    val gener = new Button {
      text = "Generate simmetric matrix A where B is E-matrix"

    }
    val butE = new Button() {
      //size = new Dimension(50, 50)
      text = "Show Eigenvectors"
    }
    val butV = new Button {
      text = "Show Eigenvalues"

    }
    val time = new Label {
      text = "Time"

    }
    val showA = new Button {
      text = "show A"

    }
    val showB = new Button {
      text = "show B"

    }
    val fd: JFileChooser = new JFileChooser
    val selectFiles = new Button("Select from file A matrix")
    val selectFilesB = new Button("Select from file B matrix")
    val radioG = new RadioButton("Generate")
    val radioF = new RadioButton("Load from file")
    butV.enabled_=(false)
    butE.enabled_=(false)
    val radios = List(radioG, radioF)
    val mutex = new ButtonGroup
    mutex.buttons ++= radios
    buttonA.enabled_=(false)
    buttonB.enabled_=(false)
    gener.enabled_=(false)
    maxDim.enabled_=(false)
    run.enabled_=(false)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new Label("Solving eigenvalue problem AX = lambdaBX.")
      contents += new Label("A and B is nxn real  symmetric matixes")
      //contents += new Label("A and B is nxn real  symmetric matixes")
      contents += new Label("X is nxm eigenvectros to find")
      contents += new Label(" Number of eigenvectors should be  m << n, where n is a size of A-matrix ")
      contents += label
      contents += new Label("Check what kind of data you want")
      contents ++= radios
      contents += selectFiles
      contents += selectFilesB
      contents += new Label("Matrix A")
      contents += buttonA
      contents += new Label("Matrix B")
      contents += buttonB
      contents += new Label("Enter the number of eigenvectors")
      contents += maxEigs
      contents += new Label("Enter the dimension of generated matrix")
      contents += maxDim
      contents += gener


      contents += run
      contents += new Label("Answer")

      contents += time
      contents += butE
      contents += butV
      contents += showA
      contents += showB
      contents += but
      border = Swing.EmptyBorder(100, 100, 100, 100)
    }
    listenTo(buttonA)
    listenTo(buttonB)
    listenTo(run)
    listenTo(but)
    listenTo(butE)
    listenTo(butV)
    listenTo(gener)
    listenTo(radioF)
    listenTo(radioG)
    listenTo(showA)
    listenTo(showB)
    listenTo(selectFiles)
    listenTo(selectFilesB)
    reactions += {
      case ButtonClicked(`radioG`) => {
        try {
          buttonA.enabled_=(false)
          buttonB.enabled_=(false)
          gener.enabled_=(true)
          maxDim.enabled_=(true)
          run.enabled_=(true)

        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`selectFiles`) => {
        try {

          fd.showOpenDialog(new JFrame)
          val inp: File = fd.getSelectedFile
          if (inp != null) aPath = inp.getAbsolutePath
          a = Matrix.fromFile(aPath)
          label.text = "Data A loaded "
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`selectFilesB`) => {
        try {

          fd.showOpenDialog(new JFrame)
          val inp: File = fd.getSelectedFile
          if (inp != null) bPath = inp.getAbsolutePath
          a = Matrix.fromFile(bPath)
          label.text = "Data B loaded "
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`radioF`) => {
        try {
          buttonA.enabled_=(true)
          run.enabled_=(true)
          buttonB.enabled_=(true)
          gener.enabled_=(false)
          maxDim.enabled_=(false)
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`buttonB`) => {
        try {
          bb = Matrix.fromFile("B.txt")
          label.text = "Data B loaded "
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`butE`) => {
        val desk = Desktop.getDesktop();
        if (Desktop.isDesktopSupported()) {
          if (desk.isSupported(Desktop.Action.OPEN)) {

            desk.open(new File("outputE.txt"))

          }

          // Выводим ошибку
        }


      }
      case ButtonClicked(`showA`) => {
        try {

          val desk = Desktop.getDesktop();
          if (Desktop.isDesktopSupported()) {
            if (desk.isSupported(Desktop.Action.OPEN)) {
              if (!gener.enabled)
                desk.open(new File("A.txt"))
              else desk.open(new File("generatedMatrixA.txt"))
            }

            // Выводим ошибку
          }
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }

      }
      case ButtonClicked(`showB`) => {
        try {

          val desk = Desktop.getDesktop();
          if (Desktop.isDesktopSupported()) {
            if (desk.isSupported(Desktop.Action.OPEN)) {

              if (!gener.enabled)
                desk.open(new File("B.txt"))
              else desk.open(new File("genMatrixB.txt"))

            }

            // Выводим ошибку
          }
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }

      }
      case ButtonClicked(`butV`) => {
        val desk = Desktop.getDesktop();
        if (Desktop.isDesktopSupported()) {
          if (desk.isSupported(Desktop.Action.OPEN)) {

            desk.open(new File("outputV.txt"))

          }

          // Выводим ошибку
        }


      }
      case ButtonClicked(`gener`) => {
        try {
          val dim: Int = maxDim.text.toInt
          if(dim>1001)throw new Exception("Please use dimension smaller than 1000")

          val g: Generator = new Generator(dim, 3)


          a = g.generateMatrix


          //Matrix n = new Generator(5, 1).generateMatrix();
          bb = g.eye(dim)
          label.text = "Matrix generated!"
          a.saveToFile("generatedMatrixA.txt")


          bb.saveToFile("genMatrixB.txt")
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }


      case ButtonClicked(`buttonA`) => {
        try {
          a = Matrix.fromFile("A.txt")
          label.text = "Data A loaded "
        }
        catch {
          case ioe: IOException => {
            label.text = "Error while read file!"
          }
          case ilarg: IllegalArgumentException => {
            label.text = "Wrong arguments!"
          }
          case e: Exception => {
            label.text = e.getMessage
          }
        }
      }
      case ButtonClicked(`run`) => {
        val t: Thread = new Thread(new Runnable {
          def run {
            try {
              try {
                val maxEV = maxEigs.text.toInt
                if (maxEV > 1001) throw new Exception("Too many vectors to compute(try smaller than 500).")
                val time1 = System.currentTimeMillis()
                if (maxEV > a.row_count) throw new Exception("Vectors to compute more than size of matrix!")
                time.text ="Data started to compute..."
                butV.enabled_=(false)
                butE.enabled_=(false)
                val (e: Matrix, v: Matrix) = IterateEigen.subspaceIter(a, bb, -0.00001, 1e-6, maxEV)
                val time2 = System.currentTimeMillis()
                butV.enabled_=(true)
                butE.enabled_=(true)
                e.saveToFile("outputE.txt")
                v.saveToFile("outputV.txt")
                time.text = "Time of computing " + (time2 - time1) + " ms" + "\n Approximate = " + 3*maxEV*a.row_count*(2*a.cols+maxEV+1)

                label.text = "Data computed "

              }
              catch {
                case ioe: IOException => {
                  label.text = "Error while read file!"
                }
                case ilarg: IllegalArgumentException => {
                  label.text = "Wrong arguments!"
                }
                case e: Exception => {
                  label.text = e.getMessage
                }
              }
            }
            catch {
              case e: IOException => {
                e.printStackTrace
              }
            }
          }
        })
        t.start

      }
      case ButtonClicked(`but`) => {
        close()
        IterateEigen.closes()
      }

    }


  }

}
