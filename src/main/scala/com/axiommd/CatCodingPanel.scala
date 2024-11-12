package com.axiommd

import typings.vscode.mod as vscode
import typings.vscode.anon.Dispose
import typings.vscode.Thenable

import scala.collection.immutable
import scala.util.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.UndefOr
import javax.swing.table.TableColumn
import upickle.default._
import javax.xml.catalog.CatalogResolver


val cats = Map(
  "Coding Cat" -> "https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif",
  "Compiling Cat" -> "https://media.giphy.com/media/mlvseq9yvZhba/giphy.gif",
  "Testing Cat"-> "https://media.giphy.com/media/3oriO0OEd9QIDdllqo/giphy.gif"
)
class CatCodingPanel (private var _panel: vscode.WebviewPanel,private val _extensionUri: vscode.Uri) :
  // Set the webview's initial html content
  _update()
  // Listen for when the panel is disposed
	// This happens when the user closes the panel or when the panel is closed programmatically
  _panel.onDidDispose((a:Unit) => dispose(), (), this._disposables) 


  _panel.onDidChangeViewState((e) => {
      if(_panel.visible){
        _update()
      }
    }, null, _disposables)

  _panel.webview.onDidReceiveMessage(processageMsg, null, _disposables)  

  def processageMsg(m:Any):Any =
    val message = m.asInstanceOf[Message]
    message.typ match {
      case "alert" => vscode.window.showErrorMessage(message.value)
      case _ => ()
    }


  private val _disposables: js.Array[vscode.Disposable] = new js.Array()

  private def  _update() = 
    val webview = this._panel.webview
    _panel.viewColumn match {
      case vscode.ViewColumn.Two => 
        _updateForCat(webview,"Compiling Cat")
      case vscode.ViewColumn.Three =>
        _updateForCat(webview,"Testing Cat")
      case vscode.ViewColumn.One  =>    
        _updateForCat(webview,"Coding Cat")
      case _ => _updateForCat(webview,"Coding Cat")

    }


  private def _updateForCat(webview: vscode.Webview, catName: String)  =
    _panel.title = catName
    _panel.webview.html = _getHtmlForWebview(webview,cats(catName))

  private def _getHtmlForWebview(webview: vscode.Webview, catGifPath: String) = 
    val scriptPathOnDisk = vscode.Uri.joinPath(_extensionUri, "media", "main.js")
    val scriptUri = webview.asWebviewUri(scriptPathOnDisk)
    val styleResetPath = vscode.Uri.joinPath(_extensionUri, "media", "reset.css")
    val stylesPathMainPath = vscode.Uri.joinPath(_extensionUri, "media", "vscode.css")

    val stylesResetUri = webview.asWebviewUri(styleResetPath)
    val stylesMainUri = webview.asWebviewUri(stylesPathMainPath)
    val nonce = Nonce()

    s"""
      <!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">

				<!--
					Use a content security policy to only allow loading images from https or from our extension directory,
					and only allow scripts that have a specific nonce.
				-->
				<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource}; img-src ${webview.cspSource} https:; script-src 'nonce-${nonce}'; connect-src 'self' http://localhost:8080;">

				<meta name="viewport" content="width=device-width, initial-scale=1.0">

				<link href="${stylesResetUri}" rel="stylesheet">
				<link href="${stylesMainUri}" rel="stylesheet">

				<title>Cat Coding</title>
			</head>
			<body>
      Hello CATWORLD!!!!!!!!!!!!!!!!!!!!!!!!!!!
				<img src="${catGifPath}" width="300" />
				<h1 id="lines-of-code-counter">0</h1>
        <div id="app"></div>

				<script nonce="${nonce}" src="${scriptUri}"></script>
			</html>

    """


  def doRefactor() = 
    this._panel.webview.postMessage(write(Message("refactor","refactor" )))  

  def dispose() =
    CatCodingPanel.currentPanel = None

    this._panel.dispose()
    while(this._disposables.length > 0){
      val x = this._disposables.pop()
      x.dispose()
    }




object CatCodingPanel:
  var currentPanel: Option[CatCodingPanel] = None
  val viewType= "catCoding"


  def createOrShow(extensionUri: vscode.Uri) =
    Console.log("createOr Show!!!")
    val column = vscode.window.activeTextEditor.toOption.flatMap{editor => editor.viewColumn.toOption}
    Console.log(s"column: ${column}")

    // If we already have a panel, show it.
    val resultExists =  for{
      panel <- CatCodingPanel.currentPanel
      col <- column
    }
    yield {
        Console.log("REVEALING PANEL THAT ALREADY EXISTS")
        panel._panel.reveal(col)
    } 

    Console.log(s"resultExists: ${resultExists}")   
    resultExists.getOrElse{
      val newPanel = createNewPanel()
      Console.log(s"newPanel: ${newPanel}")
      currentPanel = Some(new CatCodingPanel(newPanel, extensionUri))
    }

    /** 
      orElse{
        Console.log("CREATING NEW PANEL")
        createNewPanel
      }
    */

    // Otherwise, create a new panel.
    def createNewPanel() =  
      Console.log("Creating new panel")
      vscode.window.createWebviewPanel(
        viewType,
        "Cat Coding",
        column.getOrElse( vscode.ViewColumn.One),
        WebviewOptions.getWebviewOptions(extensionUri).asInstanceOf[vscode.WebviewOptions & vscode.WebviewPanelOptions]
      );



  def revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) =
    currentPanel = Some(new CatCodingPanel(panel, extensionUri))




