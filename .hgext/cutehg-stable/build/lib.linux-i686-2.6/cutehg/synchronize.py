#!/usr/bin/env python

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Tom Burdick <thomas.burdick@gmail.com> 
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

import re
import urlparse
from PyQt4 import QtCore, QtGui, uic
from dialogs import Ui_syncDialog

class SynchronizeDialog(QtGui.QDialog, Ui_syncDialog):
    """ 
    Base Class for Push and Pull Dialogs

    The implementation is basically the same for both with the exception of the mercurial command performed.
    So to realize this in a sane way there is one synchronize dialog and class that is used (either composition or inheritance)
    by the push and pull dialogs. I suppose clone could probably use the same dialog along with some additional options as well.
    """
    def __init__(self, parent=None):
        QtGui.QDialog.__init__(self, parent)

        # Set up the user interface from Designer
        self.setupUi(self)
    
        # create regexp's used for parsing a url
        self._httpRegExp = re.compile('^[Hh][Tt][Tt][Pp]://.*')
        self._httpsRegExp = re.compile('^[Hh][Tt][Tt][Pp][Ss]://.*')
        self._sshRegExp = re.compile('^[Ss][Ss][Hh]://.*')
        self._unixfolderRegExp = re.compile('^(/).*')
        self._windowsfolderRegExp = re.compile('^([A-Za-z]:[\\/]).*')
        self._remoteRegExp = re.compile('^[A-Za-z]+://((\w+)(:\S+)?@)?([A-Za-z][\w\.]*)?(:([0-9]+))?(/.*)?')

        # toggle for which thing to update, form or url
        self._doNotUpdate = False

        # get the line edit from the combo box
        self.urlLineEdit = self.urlComboBox.lineEdit()
        self._url = ""

        self.toggleLog(QtCore.Qt.Unchecked)

        # handle events
        assert self.connect(self.showLog, QtCore.SIGNAL("stateChanged(int)"), self.toggleLog)
        assert self.connect(self.actionSync, QtCore.SIGNAL("activated()"), self.doSynchronize)
        assert self.connect(self.actionCancel, QtCore.SIGNAL("activated()"), self.close)
        assert self.connect(self.browseButton, QtCore.SIGNAL("clicked()"), self.onBrowse)
        assert self.connect(self.protocolComboBox, QtCore.SIGNAL("currentIndexChanged(int)"), self.onProtocolChanged)
        assert self.connect(self.urlLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateForm)
        assert self.connect(self.hostLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateUrl)
        assert self.connect(self.portLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateUrl)
        assert self.connect(self.folderLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateUrl)
        assert self.connect(self.userLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateUrl)
        assert self.connect(self.passwordLineEdit, QtCore.SIGNAL("textChanged(const QString&)"), self.updateUrl)


    def enable(self):
        self.syncButton.setEnabled(True)
        self.cancelButton.setEnabled(True)

    def disable(self):
        self.syncButton.setEnabled(False)
        self.cancelButton.setEnabled(False)

    def toggleLog(self, checkstate):
        self.log.setVisible(checkstate == QtCore.Qt.Checked)

    def getUrl(self):
        return self._url

    def doSynchronize(self):
        """ push or pull from a given url, must derive for actual usage """

    def onProtocolChanged(self, index):
        index = min(1, index)
        self.optionsStackWidget.setCurrentIndex(index)

        if self.protocolComboBox.currentText().toLower() == "ssh":
            self.portLineEdit.setText("22")
        elif self.protocolComboBox.currentText().toLower() == "http":
            self.portLineEdit.setText("80")
        elif self.protocolComboBox.currentText().toLower() == "https":
            self.portLineEdit.setText("443")

        self.updateUrl()

    def updateForm(self, text=None):
        """ updates the form when the url combo line edit changes and it matches a valid regex """
        
        if self._doNotUpdate:
            return

        self._doNotUpdate = True 
       
        text = str(text)

        protocol = self.parseProtocol(text)
        if protocol == "Local":
            self.protocolComboBox.setCurrentIndex(self.protocolComboBox.findText(protocol))
            self.browseLineEdit.setText(text)

        elif protocol == "HTTP" or protocol == "HTTPS" or protocol == "SSH":
            self.protocolComboBox.setCurrentIndex(self.protocolComboBox.findText(protocol))
            self.browseLineEdit.setText("")
    
            user = self.parseUser(text)
            self.userLineEdit.setText(user)

            password = self.parsePassword(text)
            self.passwordLineEdit.setText(password)

            host = self.parseHost(text)
            self.hostLineEdit.setText(host)

            port = self.parsePort(text)
            self.portLineEdit.setText(port)
           
            location = self.parseLocation(text)
            self.folderLineEdit.setText(location)

        self._url = text
        self._doNotUpdate = False

    def parseProtocol(self, text):
        if self._httpRegExp.match(text):
            return "HTTP"
        elif self._httpsRegExp.match(text):
            return "HTTPS"
        elif self._sshRegExp.match(text):
            return "SSH"
        elif self._unixfolderRegExp.match(text):
            return "Local"
        elif self._windowsfolderRegExp.match(text):
            return "Local"
   
    def parseRemoteGroup(self, text, group):
        if self._remoteRegExp.match(text):
            results = self._remoteRegExp.search(text)
            result = results.group(group)
            if result:
                return result
            else:
                return ""
        else:
            return ""

    def parseUser(self, text):
        return self.parseRemoteGroup(text, 2)

    def parsePassword(self, text):
        return self.parseRemoteGroup(text, 3)

    def parseHost(self, text):
        return self.parseRemoteGroup(text, 4)

    def parsePort(self, text):
        return self.parseRemoteGroup(text, 6)

    def parseLocation(self, text):
        return self.parseRemoteGroup(text, 7)

        
    def updateUrl(self, text=None):
        """ updates the url when the form changes """
        
        if self._doNotUpdate:
            return

        self._doNotUpdate = True
        if self.protocolComboBox.currentIndex() == 0:
            url = self.browseLineEdit.text()
            self._url = url
        else:
            url = self.protocolComboBox.currentText().toLower()
            url = url + "://"
            self._url = url
           
            if not self.userLineEdit.text().isEmpty():
                url = url + self.userLineEdit.text()
                url = url + "@"
                
                self._url = self._url + self.userLineEdit.text()
                if not self.passwordLineEdit.text().isEmpty():
                    self._url = self._url + ":"
                    self._url = self._url + self.passwordLineEdit.text()
                self._url = self._url + "@"

            if not self.hostLineEdit.text().isEmpty():
                url = url + self.hostLineEdit.text().toLower()
                self._url = self._url + self.hostLineEdit.text().toLower()
               
            if not self.portLineEdit.text().isEmpty():
                if self.protocolComboBox.currentText().toLower() == "http" and self.portLineEdit.text() != "80":
                    url = url + ":"
                    url = url + self.portLineEdit.text()
                if self.protocolComboBox.currentText().toLower() == "https" and self.portLineEdit.text() != "443":
                    url = url + ":"
                    url = url + self.portLineEdit.text()
                if self.protocolComboBox.currentText().toLower() == "ssh" and self.portLineEdit.text() != "22":
                    url = url + ":"
                    url = url + self.portLineEdit.text()

                self._url = self._url + ":"
                self._url = self._url + self.portLineEdit.text()

            if not self.folderLineEdit.text().isEmpty():
                url = url + self.folderLineEdit.text()
                
                self._url = self._url + self.folderLineEdit.text()

        self.urlLineEdit.setText(url)
        self._doNotUpdate = False

    def onBrowse(self):
        directory = QtGui.QFileDialog.getExistingDirectory( self, 
            "Repository Location", 
            QtCore.QDir.homePath(), 
            QtGui.QFileDialog.ShowDirsOnly | QtGui.QFileDialog.DontResolveSymlinks 
            )
        self.browseLineEdit.setText(directory)
        self.updateUrl()

if __name__ == "__main__":
    import sys
    app = QtGui.QApplication( sys.argv )
    dialog = SynchronizeDialog()
    sys.exit(app.exec_())



