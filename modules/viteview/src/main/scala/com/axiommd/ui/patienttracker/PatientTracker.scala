package com.axiommd.ui.patienttracker

import com.axiom.model.shared.dto.Patient
import scala.collection.mutable
import com.axiommd.shapeless.{ShapelessFieldNameExtractor, Tuples}
import com.axiommd.ui.tableutils.{CCRowList, ColRow, GridDataT, GridT}
import com.axiommd.ui.patienttracker.TypeClass.*
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import com.raquo.laminar.api.L
import com.raquo.airstream.ownership.OneTimeOwner
import org.scalajs.dom.KeyboardEvent
import scala.scalajs.js

type PatientList = CCRowList[Patient]

trait RenderHtml:
  def renderHtml: Element

case class CellData(text: String, color: String)

case class PatientGridData(grid: GridT[Patient, CellData], colrow: ColRow, data: CellData)
  extends GridDataT[GridT[Patient, CellData], Patient, CellData](grid, colrow, data) with RenderHtml:
    def renderHtml = td(data.text, backgroundColor := data.color)

class PatientTracker() extends GridT[Patient, CellData] with RenderHtml:

  given owner: Owner = new OneTimeOwner(() => ())
  val selectedCellVar: Var[Option[ColRow]] = Var(None)
  val selectedRowVar: Var[Option[Int]] = Var(None)
  val searchQueryVar: Var[String] = Var("")
  
  // Map to track sorting direction per column (true = ascending, false = descending)
  val sortState: Var[Map[String, Boolean]] = Var(Map.empty)

  selectedCellVar.signal.foreach { setSelectedRow }
  selectedRowVar.signal.foreach { scrollToSelectedRow }

  val colHeadersVar: Var[List[String]] = Var(ShapelessFieldNameExtractor.fieldNames[Patient].take(15))

  def columns(row: Int, p: Patient) =
    val c = mutable.IndexedSeq(CellDataConvertor.derived[Patient].celldata(p)*).take(15)
    c(0) = c(0).copy(text = s"**${c(0).text}*", color = "pink")
    c.toList

  private def setSelectedRow(cr: Option[ColRow]) =
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

  override def cctoData(row: Int, cc: Patient): List[CellData] = columns(row, cc)

  def searchFilterFunction(rowData: scala.collection.mutable.IndexedSeq[(GridT[Patient, CellData], ColRow, CellData)]): Boolean = {
    val query = searchQueryVar.now().toLowerCase
    val rowContent = rowData.map(_._3.text).mkString(" ").toLowerCase
    rowContent.contains(query)
  }

  def renderHtml: L.Element =
    div(
      cls := "table-container",
      div(
        cls := "sticky-bar",
        label("Search: "),
        input(
          typ := "text",
          placeholder := "Search patients...",
          inContext { thisNode =>
            onInput.mapTo(thisNode.ref.value) --> searchQueryVar
          }
        )
      ),
      table(
        cls := "sticky-table",
        onKeyDown --> tableKeyboardHandler,
        thead(
          children <-- colHeadersVar.signal.map { headers =>
            List(
              tr(
                headers.map { header =>
                  th(
                    cls := "sticky-header",
                    div(
                      span(header),
                      button(
                        cls := "sort-button",
                        onClick --> { _ => handleSort(header) },
                        child <-- sortState.signal.map { sortMap =>
                          val direction = sortMap.getOrElse(header, true)
                          span(
                            cls := "sort-indicator",
                            if (direction) cls := "asc" else cls := "desc" // Ascending or descending indicator
                          )
                        }
                      )
                    )
                  )
                }
              )
            )
          }
        ),
        tbody(
          children <-- gcdVar.signal.combineWith(searchQueryVar.signal).map { case (rowList, query) =>
            rowList.filter { rowData => searchFilterFunction(rowData) }
          }.map { filteredRows =>
            filteredRows.map(rowData => row(rowData))
          }
        )
      )
    )

  def row(cols: Row) =
    val pgd = cols.map { Tuples.from[PatientGridData](_) }
    tr(
      idAttr := s"row-${cols.head._2.row}",
      backgroundColor <-- selectedRowVar.signal.map { selRow =>
        selRow match
          case Some(row) if row == cols.head._2.row => "blue"
          case _ => "black"
      },
      pgd.map { c => this.tableCell(c.colrow) }
    )

  def tableCell(colRow: ColRow): HtmlElement =
    val patientGridData = data(colRow).map { Tuples.from[PatientGridData](_) }
    td(
      tabIndex := colRow.row * 9000 + colRow.col,
      color := patientGridData.map(_.data.color).getOrElse("black"),
      onKeyDown --> keyboardHandler,
      onMouseUp.mapTo(colRow).map(Some(_)) --> selectedCellVar.writer,
      patientGridData.map(_.data.text).getOrElse("---")
    )

  def tableKeyboardHandler(e: KeyboardEvent): Unit =
    e.keyCode match
      case 40 | 38 | 37 | 39 | 32 => e.preventDefault() // Prevent default scrolling behavior
      case _ => ()

  def keyboardHandler(e: KeyboardEvent): Unit =
    val selectedCellOpt = selectedCellVar.now()
    def conditionalUpdate(vector: ColRow): Unit =
      selectedCellOpt.foreach { currentColRow =>
        val newColRow = currentColRow.add(vector)
        if inBounds(newColRow) then selectedCellVar.set(Some(newColRow))
      }
    e.keyCode match
      case 40 => conditionalUpdate(ColRow(0, 1))  // down
      case 38 => conditionalUpdate(ColRow(0, -1)) // up
      case 37 => conditionalUpdate(ColRow(-1, 0)) // left
      case 39 => conditionalUpdate(ColRow(1, 0))  // right
      case _ => ()

  /**
   * Handles sorting by the given column name.
   * Toggles between ascending and descending states.
   */
  def handleSort(columnName: String): Unit = {
    // Update the sort state to toggle sorting direction
    sortState.update { currentState =>
      val currentDirection = currentState.getOrElse(columnName, true) // Default to ascending
      currentState.updated(columnName, !currentDirection) // Toggle direction
    }

    val direction = sortState.now()(columnName) // true = ascending, false = descending
    val headers = colHeadersVar.now()
    val colIndex = headers.indexOf(columnName)

    if (colIndex >= 0) {
      // Retrieve the current data
      val currentRows = gcdVar.now()

      // Sort the rows based on the cell text at colIndex
      val sortedRows = currentRows.sortBy { row =>
        val cellData = row(colIndex)._3
        cellData.text.toLowerCase
      }

      // If descending, reverse the sorted list
      val finalRows = if (!direction) sortedRows.reverse else sortedRows

      // Update gcdVar with the sorted data
      gcdVar.set(finalRows)
    }

    println(s"Sorting by column: $columnName, direction: ${if (direction) "Ascending" else "Descending"}")
  }
  