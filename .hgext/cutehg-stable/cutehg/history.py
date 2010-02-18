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

import pdb
import cProfile
from dialogs import Ui_HistoryDialog
from PyQt4 import QtCore, QtGui
from mercurial import ui, repo, commands, hg, node, util
from views import HistoryView, StatusView, DiffView
from runners import *
from pull import PullDialog
from push import PushDialog

class HistoryDialog(QtGui.QMainWindow, Ui_HistoryDialog):
    def __init__(self, ui, repo):
        QtGui.QMainWindow.__init__(self)
        self.setupUi(self)

        self._ui = ui
        self._repo = repo
        self._runner = None
        self._revision = None

        self.showDiff = False
        self.showStatus = False
        self.historyView = HistoryView(ui, repo)
        self.statusView = StatusView(ui, repo)
        self.diffView = DiffView(ui, repo)
        self.setCentralWidget(self.historyView)
        
        self.vsplitter = QtGui.QSplitter()
        self.vsplitter.setOrientation(QtCore.Qt.Vertical)
        
        self.hsplitter = QtGui.QSplitter()
        self.hsplitter.setOrientation(QtCore.Qt.Horizontal)
        
        self.initMenus()
        self.initEnableDisable()
        self.initConnect()

    def initMenus(self):
        return
        pushMenu = QtGui.QMenu()
        pushMenu.addAction(self.actionDefaultPush)
        pushMenu.addAction(self.actionPush)
        pushButton = self.hgActions.widgetForAction(self.actionDefaultPush)
        pushButton.setMenu(pushMenu)
        pushButton.setPopupMode(QtGui.QToolButton.MenuButtonPopup)

        pullMenu = QtGui.QMenu()
        pullMenu.addAction(self.actionPull)
        self.actionDefaultPull.setMenu(pullMenu)

    def initEnableDisable(self):
        ''' append all the objects that need to be disabled and enabled  to a list '''
        self._endisWidgets = []
        ed = self._endisWidgets
        ed.append(self.actionUpdate)
        ed.append(self.actionCommit)
        ed.append(self.actionPull)
        ed.append(self.actionPush)
        ed.append(self.actionMerge)
        ed.append(self.actionRollback)
        ed.append(self.actionBackout)

    def initConnect(self):
        triggered = QtCore.SIGNAL("triggered()")
        toggled = QtCore.SIGNAL("toggled(bool)")
        assert self.connect( self.actionCommit, triggered, self.onCommit )
        assert self.connect( self.actionRollback, triggered, self.onRollback )
        assert self.connect( self.actionDefaultPull, triggered, self.onPull )
        assert self.connect( self.actionDefaultPush, triggered, self.onPush )
        #assert self.connect( self.actionPull, triggered, self.onPull )
        #assert self.connect( self.actionPush, triggered, self.onPush )
        assert self.connect( self.actionMerge, triggered, self.onMerge )
        assert self.connect( self.actionUpdate, triggered, self.onUpdate )
        assert self.connect( self.actionBackout, triggered, self.onBackout )
        assert self.connect( self.actionDiff, triggered, self.onDiff )
        assert self.connect( self.actionServe, triggered, self.onServe )
        assert self.connect( self.actionShowStatus, toggled, self.onShowStatusToggle )
        assert self.connect( self.actionShowDiff, toggled, self.onShowDiffToggle )
        assert self.connect( self.historyView, QtCore.SIGNAL("selectionChanged(int)"), self.statusView.update)
        assert self.connect( self.historyView, QtCore.SIGNAL("selectionChanged(int)"), self.diffView.update)

    def enable(self):
        ''' enable all the buttons '''
        for edobj in self._endisWidgets:
            edobj.setEnabled(True)

    def disable(self):
        ''' disable all the buttons '''
        for edobj in self._endisWidgets:
            edobj.setEnabled(False)

    def onSelectionChange(self):
        ''' update various parts of the gui when selection changes
            update the toolbar button enabled/disabled status
            update the status view
            update the diff view
        '''

    def onRefresh(self):
        ''' reload the repository information in the models
            should almost never be needed since the history, status, and diff view watch files
            for changes 
        '''
    
    def setupPanes(self):
        # hold on to referrences of the old splitters so widgets don't actually get deleted
        oldvsplitter = self.vsplitter
        oldhsplitter = self.hsplitter
        self.historyView.setParent(self)
        self.diffView.setParent(self)
        self.statusView.setParent(self)
        if self.showStatus and self.showDiff:
            self.hsplitter = QtGui.QSplitter()
            self.hsplitter.setOrientation(QtCore.Qt.Horizontal)
            self.hsplitter.addWidget(self.statusView)
            self.hsplitter.addWidget(self.diffView)
            self.vsplitter = QtGui.QSplitter()
            self.vsplitter.setOrientation(QtCore.Qt.Vertical)
            self.vsplitter.addWidget(self.historyView)
            self.vsplitter.addWidget(self.hsplitter)
            self.setCentralWidget(self.vsplitter)
        elif self.showStatus and not self.showDiff:
            self.vsplitter = QtGui.QSplitter()
            self.vsplitter.setOrientation(QtCore.Qt.Vertical)
            self.vsplitter.addWidget(self.historyView)
            self.vsplitter.addWidget(self.statusView)
            self.setCentralWidget(self.vsplitter)
        elif not self.showStatus and self.showDiff:
            self.vsplitter = QtGui.QSplitter()
            self.vsplitter.setOrientation(QtCore.Qt.Vertical)
            self.vsplitter.addWidget(self.historyView)
            self.vsplitter.addWidget(self.diffView)
            self.setCentralWidget(self.vsplitter)
        else:
            self.setCentralWidget(self.historyView)
    
        
    def onShowDiffToggle(self, show):
        ''' show/hide the diff pane '''
        self.showDiff = show
        self.setupPanes()
        
    def onShowStatusToggle(self, show):
        ''' show/hide the status pane '''
        self.showStatus = show
        self.setupPanes()

    def onRollback(self):
        ''' attempt to revert the last command '''

        
    def onCommit(self):
        ''' commit changes '''
    
    def onUpdate(self):
        ''' update working directory to a revision '''
        self.disable()
        self._revision = self.historyView.selectedRevision()
        self._runner = UpdateRunner( self.enable, self, self._ui, self._repo, self._revision, False)
       
    def onPullDefault(self):
        self.disable()
        self._runner = PullRunner( self.enable, self, self._ui, self._repo)
        
    def onPull(self):
        ''' bring pull a specific revision and all parents '''
        pullDialog = PullDialog(self._ui, self._repo, self)
        pullDialog.setModal(True)
        pullDialog.exec_()
        pullDialog = None
        
    def onPushDefault(self):
        self.disable()
        self._runner = PullRunner( self.enable, self, self._ui, self._repo )
        
    def onPush(self):
        ''' push all changes to a specific location '''
        pushDialog = PushDialog(self._ui, self._repo, self)
        pushDialog.setModal(True)
        pushDialog.exec_()
        pushDialog = None
        
    def onDiff(self):
        ''' display a diff in an external viewer '''
        # execute whatever hg vdiff -r xyz -r abc would do, if no vdiff is defined show a text box with the diff in it
        
    def onMerge(self):
        ''' merge the selected revision with the working revision '''
        
    def onBackout(self):
        ''' backout a selected changeset '''
        
    def onServe(self):
        ''' show the serve dialog, notably if the user closes it the server keeps running
        the next time the dialog is show it will show the server is running still
        '''
if __name__ == "__main__":
    import os
    import sys
    import utils
    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=False, traceback=False)
    path = len(sys.argv) > 1 and sys.argv[1] or os.getcwd()
    path = utils.findrepo(os.path.abspath(path))
    repo = hg.repository(u, path)
    
    dialog = HistoryDialog(u, repo)
    dialog.show()
    cProfile.run('app.exec_()', 'appprof')
    import pstats
    p = pstats.Stats('appprof')
    p.strip_dirs().sort_stats(-1).print_stats()

