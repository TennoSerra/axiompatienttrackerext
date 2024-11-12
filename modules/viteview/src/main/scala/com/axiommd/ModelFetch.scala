package com.axiommd
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import com.axiom.model.shared.dto.Patient
import com.axiommd.shapeless.TableColProperties
import io.laminext.fetch._
import org.scalajs.dom.AbortController
import com.axiommd.shapeless._
import com.axiommd.shapeless.ShapelessFieldNameExtractor.fieldNames

import com.raquo.airstream.ownership.OneTimeOwner
  
object ModelFetch :
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  import zio.json._

  import io.laminext.fetch._
  import scala.concurrent.{Future,Promise}
  import org.scalajs.dom.AbortController
  import scala.collection.mutable

  val abortController = new AbortController()

  def fetchPatients = 
    import java.time._ //cross scalajs and jvm compatible
    import com.axiom.model.shared.dto.Patient 
    import com.axiommd.shapeless.ShapelessFieldNameExtractor
    
    Fetch.get("http://localhost:8080/patientsjson").future.text(abortController)
      .map(s => s.data.fromJson[List[Patient]])
      .map(r => r.toOption.getOrElse(Nil))

    




