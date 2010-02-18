#!/usr/bin/env python
# -*- coding: utf-8 -*-

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

'''
Runners are thread wrappers for Commands

The goal here is to run mercurial commands (mostly wrapped by Commands) in a thread and have a easily
reusable module that can be used in multiple dialogs. This way the dialogs can share a great deal of code and functionality.
'''

import traceback
from commands import *
from PyQt4 import QtCore, QtGui

class CommandRunner(QtCore.QThread):
    ''' A command runner base class that executes a Command in a thread '''
    def __init__(self, name, callback, parent=None):
        QtCore.QThread.__init__(self, parent)
        self.callback = callback
        self.name = name
        self.success = False 
        assert self.connect( self, QtCore.SIGNAL("finished()"), self.callback )
        assert self.connect( self, QtCore.SIGNAL("error"), self.showErrorDialog, QtCore.Qt.QueuedConnection )
        assert self.connect( self, QtCore.SIGNAL("result"), self.showResultDialog, QtCore.Qt.QueuedConnection )

    def run(self):
        ''' the method run while the thread is running '''
        try:
            self.command()
            self.result()
        except Exception, e:
            self.error(str(e))
    
    def error(self, msg):
        ''' emits a signal which is then passed over a queue to the event loop '''
        self.emit(QtCore.SIGNAL("error"), msg)
        self.errormsg = msg
    
    def showErrorDialog(self, msg):
        ''' pop up a dialog reporting an error '''            
        dialog = QtGui.QMessageBox.critical(self.parent(), ("Mercurial %s Error" % self.name), msg )
        
    def result(self, msg=None):
        if not msg:
            msg = self.name + " Completed Successfully"
        self.emit(QtCore.SIGNAL("result"), msg)

    def showResultDialog(self, msg):
        dialog = QtGui.QMessageBox.information(self.parent(), ("Mercurial %s Results" % self.name), msg )



class UpdateRunner(CommandRunner):
    def __init__(self, callback, parent, ui, repo, rev=None, force=False):
        CommandRunner.__init__(self, "Update", callback, parent)
        
        if not rev:
            self._rev='tip'
        else:
            self._rev=rev

        self.command = Update(ui, repo, rev, force)
        self.perform = True
        assert self.connect( self, QtCore.SIGNAL("force"), self.showForceDialog, QtCore.Qt.BlockingQueuedConnection )
        self.start()
    
    def run(self):
        ''' perform the update in a thread '''
        try:
            self.command.test()
        except UncommittedChangesError, e:
            self.force()
        except Exception, e:
            self.error(str(e))
            self.perform = False
            
        if self.perform:
            try:
                self.command.perform()
                msg = "%s to %s Completed Successfully" % (self.name, str(self._rev))
                self.result(msg) 
            except Exception, e:
                self.error(str(e))

    def force(self):
        self.emit(QtCore.SIGNAL("force"))
        
    def showForceDialog(self):
        results = QtGui.QMessageBox.question( self.parent(), "Mercurial Update Warning", "There are uncommitted changes, updating will destroy changes. Update anyways?", QtGui.QMessageBox.Yes | QtGui.QMessageBox.No )
        if results == QtGui.QMessageBox.Yes:
            self.command.force = True
            self.perform = True
            self.errormsg = None
        else:
            self.perform = False
        
class PullRunner(CommandRunner):
    def __init__(self, callback, parent, ui, repo, url=None, rev=None, force=False):
        CommandRunner.__init__(self, "Pull", callback, parent)
        self.command = Pull(ui, repo, url, rev, force)
        self._password = None
        self.start()
        
class PushRunner(CommandRunner):
    def __init__(self, callback, parent, ui, repo, url=None, rev=None, force=False):
        CommandRunner.__init__(self, "Push", callback, parent)
        self.command = Push(ui, repo, url, rev, force)
        self.perform = True
        assert self.connect( self, QtCore.SIGNAL("force"), self.showForceDialog, QtCore.Qt.BlockingQueuedConnection )
        self.start()
        
    def run(self):
        ''' perform the push in a thread '''
        try:
            self.command.test()
        except NewRemoteHeadsError, e:
            self.force()
        except Exception, e:
            self.error(str(e))
            self.perform = False

        if self.perform:
            try:
                self.command.perform()
                self.result()
            except Exception, e:
                self.error(str(e))
        
    def force(self):
        self.emit(QtCore.SIGNAL("force"))
        
    def showForceDialog(self):
        results = QtGui.QMessageBox.question( self.parent(), "Mercurial Push Warning", "Pushing will create new remote heads, push anyways?", QtGui.QMessageBox.Yes | QtGui.QMessageBox.No )
        if results == QtGui.QMessageBox.Yes:
            self.command.force = True
            self.perform = True
        else:
            self.perform = False

