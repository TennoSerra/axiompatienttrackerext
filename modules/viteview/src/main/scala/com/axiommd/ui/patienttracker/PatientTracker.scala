package com.axiommd.ui.patienttracker

import com.axiom.model.shared.dto.Patient
import scala.collection.mutable
import com.axiommd.shapeless.{ShapelessFieldNameExtractor,Tuples}
import java.time.*
import com.axiommd.ui.tableutils.{CCRowList,ColRow,GridDataT,GridT}
import com.axiommd.ui.patienttracker.TypeClass.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import com.axiommd.ModelFetch
import com.raquo.laminar.api.L
import com.raquo.airstream.ownership.OneTimeOwner
import org.scalajs.dom.KeyboardEvent
import io.bullet.borer.derivation.key
import scala.scalajs.js

type PatientList = CCRowList[Patient]


trait RenderHtml :
  def renderHtml:Element
  


case class CellData(text:String,color:String) 


case class PatientGridData(grid: GridT[Patient,CellData],colrow:ColRow, data:CellData) 
  extends GridDataT[GridT[Patient,CellData],Patient,CellData](grid,colrow,data) with RenderHtml :
    def renderHtml = td(data.text,backgroundColor:=data.color)


class PatientTracker() extends GridT [Patient,CellData] with RenderHtml:


  given owner:Owner = new OneTimeOwner(()=>())
  val selectedCellVar:Var[Option[ColRow]] = Var(None)
  val selectedRowVar:Var[Option[Int]] = Var(None)

  selectedCellVar.signal.foreach{ setSelectedRow }
  selectedRowVar.signal.foreach { scrollToSelectedRow }

  
  val colHeadersVar:Var[List[String]] = Var(ShapelessFieldNameExtractor.fieldNames[Patient].take(15))

  def columns(row:Int,p:Patient) =  
    val c = mutable.IndexedSeq(CellDataConvertor.derived[Patient].celldata(p)*).take(15)
    c(0) = c(0).copy(text = s"**${c(0).text}*", color = "pink")
    c.toList

  private def setSelectedRow(cr:Option[ColRow])  = 
    cr match
      case Some(sel) => 
          selectedRowVar.set(Some(sel.row))
      case _ => 
          selectedRowVar.set(None)


  private def scrollToSelectedRow(rowIdxOpt: Option[Int]): Unit = 
    rowIdxOpt match {
      case Some(rowIdx) =>
        Option(dom.document.getElementById(s"row-$rowIdx")).foreach { element =>
          val rect = element.getBoundingClientRect()
          val isInView = rect.top >= 0 && rect.bottom <= dom.window.innerHeight

          if (!isInView) {
            element.asInstanceOf[js.Dynamic].scrollIntoView(
              js.Dynamic.literal(
                behavior = "smooth",
                block = "nearest"
              )
            )
          }
        }
      case None => // Do nothing
    }

  override def cctoData(row:Int,cc:Patient):List[CellData] = columns(row,cc)


  def renderHtml: L.Element = 
    def headerRow(s:List[String]) = 
      List(tr(
          s.map (s => th(s))
        )
      )

    table(
      onKeyDown --> tableKeyboardHandler,//prevents default scrolling behaviour from various key strokes
      thead(
        children <-- colHeadersVar.signal.map{headerRow(_) }
      ),
      tbody(
        children <-- gcdVar.signal.map{ 
          (rowList:GCD) => rowList.map(tup => row(tup))
        }
      )
  )
  
  def row(cols:Row)  = 
    
    val pgd = cols.map{Tuples.from[PatientGridData](_)}
    // val patientGridData = data(cols).map{gcdTupleToPatientGridData} 
    tr(
      idAttr := s"row-${cols.head._2.row}",
      backgroundColor <-- selectedRowVar.signal.map{ selRow => 
        selRow match
          case Some(row) if row == cols.head._2.row => "blue"
          case _ => "black"
      },
      pgd.map{c => this.tableCell(c.colrow)}
    )

  def tableCell(colRow:ColRow) : HtmlElement  =

    val patientGridData = data(colRow).map{Tuples.from[PatientGridData](_)} 

    td(
      tabIndex := colRow.row*9000 + colRow.col, //apparently I need this to capture keyboard events
      color := patientGridData.map(_.data.color).getOrElse("black"), //TODO convert tuple to case class to improve readability
      onKeyDown --> keyboardHandler,
      onMouseUp.mapTo(colRow).map(Some(_)) --> selectedCellVar.writer,
      patientGridData.map{ _.data.text }.getOrElse("---")
    )

  /**
    * event handler at the table later to prevent default behaviour from key actions
    * that can cause the web page to scroll
    *
    * @param e
    */
  def tableKeyboardHandler(e:KeyboardEvent)  =
    e.keyCode match 
      case 40 => e.preventDefault()
      case 38 => e.preventDefault()
      case 37 => e.preventDefault()
      case 39 => e.preventDefault()
      case 32 => e.preventDefault()
      case _  => ()  

    

  def keyboardHandler(e:KeyboardEvent)  =
    val selectedCellOpt = selectedCellVar.now()
    def conditionalUpdate(vector:ColRow):Unit =
      selectedCellOpt.foreach {currentColRow =>
        val newColRow = currentColRow.add(vector)
        inBounds(newColRow) match
          case true => selectedCellVar.set(Some(newColRow))
          case _ => ()
      }
    e.keyCode match
      case 40 =>  //down cursor
        conditionalUpdate(ColRow(0,1))
      case 38 => //up cursor
        conditionalUpdate(ColRow(0,-1))
      case 37 => //left cursor
        conditionalUpdate(ColRow(-1,0))
      case 39 => //right cursor
        conditionalUpdate(ColRow(-1,0))
      case 9 => //tab
        // dom.window.console.log(s"tabbed ${gd.coordinate}tab tab tab ")
      case _ => ()


