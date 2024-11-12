package com.axiommd

import typings.vscode.mod as vscode
import typings.vscode.anon.Dispose
import typings.vscode.Thenable

import scala.collection.immutable
import scala.util.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.UndefOr


import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import typings.std.stdStrings.s




object cat :

  @JSExportTopLevel("activate")
  def activate(context: vscode.ExtensionContext): Unit = 


    def catCodingStart: js.Function1[ Any, Any] = (arg) => {
      Console.log("Starting Cat Coding!!!!!!!!!!!!")
      CatCodingPanel.createOrShow(context.extensionUri)
      }
    def catCodingDoRefactor: js.Function1[ Any, Any] = (arg) =>
      Console.log("Doing Cat Coding Refactor")
      CatCodingPanel.currentPanel.foreach{ panel =>
        panel.doRefactor();
      }

    def f(a:Any):Any   = "hey" //this shows an alternative way to implmeent js.Function1[Any,Any]

    def showHello(): js.Function1[Any, Any] =
      (arg) => {
        vscode.window.showInputBox().toFuture.onComplete {
          case Success(input) => vscode.window.showInformationMessage(s"Hello Arnold $input!")
          case Failure(e)     => println(e.getMessage)
        }
      }

    val commands = List[(String,js.Function1[Any,Any])](
      ("catCoding.doRefactor", catCodingDoRefactor),
      ("catCoding.start", catCodingStart),
      ("f",f) //this shows the alternative complies with js.Function1[Any,Any]
    )

    commands.foreach { case (name, fun) =>
      context.subscriptions.push(
        vscode.commands
          .registerCommand(name, fun)
          .asInstanceOf[Dispose]
      )
    }




    if (!js.isUndefined(vscode.window.registerWebviewPanelSerializer))  {
      def deserializeWebviewPanel(webviewPanel: vscode.WebviewPanel, state: Any) =
        Future {
          Console.log(s"Got state: ${state}");
          // Reset the webview options so we use latest uri for `localResourceRoots`.
          webviewPanel.webview.options =  WebviewOptions.getWebviewOptions(context.extensionUri);
          CatCodingPanel.revive(webviewPanel, context.extensionUri);
        }.onComplete({
          case Success(_) => Console.log ("Successfully revived panel")
          case Failure(e) => Console.log(s"Failed to revive panel: ${e.getMessage()}")
        })
    }


