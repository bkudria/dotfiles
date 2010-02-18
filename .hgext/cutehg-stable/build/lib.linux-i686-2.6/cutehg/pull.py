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

import re
from PyQt4 import QtCore, QtGui
from synchronize import SynchronizeDialog
from mercurial import hg, ui, repo, commands
from runners import PullRunner
from utils import *
from runnerui import RunnerUi

class PullDialog(SynchronizeDialog):
    def __init__(self, ui, repository, parent=None):
        SynchronizeDialog.__init__(self, parent)
        self._ui = ui
        self._repository = repository
        self._cutePathRegExp = re.compile('^cutehg_([0-9]+)') 
        self._lastPathCount = 0
        self.setWindowTitle("Mercurial Pull")
        self.syncButton.setText("Pull")

        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/vcs_update.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.setWindowIcon(icon)

        # setup the url paths, obtaining a list from .hgrc paths to be the ones the combo contains
        for name, path in self._repository.ui.configitems("paths"):
            self.urlComboBox.addItem(path)
            if name == "default":
                index = self.urlComboBox.findText(path)
                self.urlComboBox.setCurrentIndex(index)
            if self._cutePathRegExp.match(name):
                results = self._cutePathRegExp.search(name)
                self._lastPathCount = int(results.group(1))

    def doSynchronize(self):
        self.disable()
        ui = RunnerUi(log=self.log, parentui=self._ui, parent=self)
        self._repository.ui = ui
        self._runner = PullRunner(self.onPullDone, self, ui, self._repository, str(self.getUrl()))

    def onPullDone(self):
        self.enable()
        if self._runner.success:
            pathsRegExp = re.compile('^\s+[paths].*')

            # save this repo as cutehg_n where n is the next number to increment to
            addPath = True 
            for name, path in self._repository.ui.configitems("paths"):
                if path == str(self.getUrl()):
                    addPath = False
                    break
            if addPath:
                name = 'cutehg_%d' % (self._lastPathCount+1)
                configfile = open(self._repository.root + '/.hg/hgrc', 'r+')
                text = configfile.read()
                configfile.close()
                configfile = open(self._repository.root + '/.hg/hgrc', 'w')
                strinsert = '[paths]\n' + name + ' = ' + str(self.getUrl())
                if text.find('[paths]') is not None:
                    newtext = strinsert.join(text.split('[paths]'))
                else:
                    newtext = text + '\n'
                    newtext = newtext + strinsert
                configfile.write(newtext)
                configfile.close()
            self.close()
            
if __name__ == "__main__":
    import os, sys
    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=True, traceback=False)
    path = len(sys.argv) > 1 and sys.argv[1] or os.getcwd()
    path = findrepo(os.path.abspath(path))
    repo = hg.repository(u, path)
    dialog = PullDialog(u, repo)
    dialog.show()
    sys.exit(app.exec_())



