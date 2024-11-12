package com.axiommd

import typings.vscode.mod as vscode

object Console :
  val outputChannel = vscode.window.createOutputChannel("My Extension")

  def log(s:String) =
    outputChannel.appendLine(s)
    outputChannel.show(preserveFocus = true)

