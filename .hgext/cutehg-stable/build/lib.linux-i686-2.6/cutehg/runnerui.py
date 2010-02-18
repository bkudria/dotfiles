#!/usr/bin/env python
# -*- coding: utf-8 -*-

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Stefan Rusek <stefan@rusek.org>
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from PyQt4 import QtCore, QtGui
from mercurial import ui
from views import CommitMessageView, qtutil

class RunnerUi(ui.ui, QtCore.QObject):
    '''
    This class provides a PyQt compatible ui class for use with mercurial
    '''
    def __init__(self, log=None, dlgwarnings=False, parentui=None, parent=None):
        ui.ui.__init__(self, parentui=parentui)
        QtCore.QObject.__init__(self, parent)

        self._parent = parent
        self._wait = QtCore.QWaitCondition()
        self._log = log
        self._dlgwarnings = dlgwarnings

        if self._log:
            self._log.setSortingEnabled(False)

        assert self.connect(self, QtCore.SIGNAL("log"), self._addLog)
        assert self.connect(self, QtCore.SIGNAL("prompt"), self._prompt)
        assert self.connect(self, QtCore.SIGNAL("message"), self._message)

    def _addLog(self, msg, err):
        item = QtGui.QListWidgetItem(msg.rstrip(), self._log)
        self._log.scrollToItem(item)
        if err:
            item.setTextColor([QtCore.Qt.darkBlue, QtCore.Qt.red][err - 1])

    def _commonWrite(self, msg, color):
        if not self._log:
            return
        self.emit(QtCore.SIGNAL("log"), ''.join(msg), color)

    def _commonMessage(self, icon, msg):
        msg = ''.join(msg)
        if icon == QtGui.QMessageBox.Critical:
            self.emit(QtCore.SIGNAL("error"), icon, msg)
        else:
            self.emit(QtCore.SIGNAL("warn"), icon, msg)
        if self._dlgwarnings:
            self.emit(QtCore.SIGNAL("message"), icon, msg)

    def _message(self, icon, msg):
        try:
            dlg = QtGui.QMessageBox(icon, "CuteHg", msg)
            dlg.exec_()
        finally:
            self._wait.wakeAll()

    def _commonPrompt(self, prompt, default, isPassword, isMultiline=False):
        self.emit(QtCore.SIGNAL("prompt"), prompt, default, isPassword, isMultiline)
        self._wait.wait(QtCore.QMutex())
        return self._temp or default

    def _prompt(self, prompt, default, isPassword, isMultiline):
        class Prompt(QtGui.QDialog):
            def __init__(self, parent):
                QtGui.QDialog.__init__(self, parent)

                self.setWindowTitle("CuteHg")
                vert = QtGui.QVBoxLayout(self)

                if prompt:
                    vert.addWidget(QtGui.QLabel(prompt))

                if isMultiline:
                    self._edit = CommitMessageView(default)
                    self._edit.ensureWidth(80)
                    self.value = self._edit.value
                else:
                    self._edit = QtGui.QLineEdit(default)
                    self.value = lambda: str(self._edit.text().toUtf8())
                    if isPassword:
                        self._edit.setEchoMode(QtGui.QLineEdit.Password)
                vert.addWidget(self._edit)

                bbox = QtGui.QDialogButtonBox(QtGui.QDialogButtonBox.Ok | QtGui.QDialogButtonBox.Cancel)
                assert self.connect(bbox, QtCore.SIGNAL("accepted()"), self.accept)
                assert self.connect(bbox, QtCore.SIGNAL("rejected()"), self.reject)
                vert.addWidget(bbox)

                self._edit.setFocus()

        dlg = Prompt(self._parent)
        self._temp = None
        try:
            if QtGui.QDialog.Accepted == dlg.exec_():
                self._temp = dlg.value()
        finally:
            self._wait.wakeAll()
            
    def write(self, *msg):
        self._commonWrite(msg, 0)

    def write_err(self, *msg):
        self._commonWrite(msg, 2)
        self._commonMessage(QtGui.QMessageBox.Critical, msg)

    def warn(self, *msg):
        self._commonWrite(msg, 1)
        self._commonMessage(QtGui.QMessageBox.Warning, msg)


    def _readline(self, prompt=''):
        return self._commonPrompt(prompt, '', False)

    def getpass(self, prompt="Password requested", default=None):
        return self._commonPrompt(prompt, default, True)

    def edit(self, text, user, forceQt=False):
        if not forceQt and self.geteditor():
            return ui.ui.edit(self, text, user)
        return self._commonPrompt("", text, False, True)

    def geteditor(self):
        '''return editor to use'''
        import os
        return (os.environ.get("HGEDITOR") or
                self.config("ui", "editor") or
                os.environ.get("VISUAL") or
                os.environ.get("EDITOR", None))

if __name__ == '__main__':
    import sys
    app = QtGui.QApplication( sys.argv )

    ui = RunnerUi()
    print ui.prompt("Type something")
    print ui.getpass()
    print ui.edit("Commit message", "", forceQt=True)
