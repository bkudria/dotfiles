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

from PyQt4 import QtCore, QtGui
from views import StatusView

from mercurial import ui, repo, commands, hg, node, util

class StatusDialog(QtGui.QDialog):
    def __init__(self, ui, repo, parent=None):
        QtGui.QDialog.__init__(self, parent)
        self.verticalLayout = QtGui.QVBoxLayout(self)
        self.statusView = StatusView(ui, repo)
        self.verticalLayout.addWidget(self.statusView)
        self.setWindowTitle("Mercurial Status")

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
    
    dialog = StatusDialog(u, repo)
    dialog.show()
    cProfile.run('app.exec_()', 'appprof')
    import pstats
    p = pstats.Stats('appprof')
    p.strip_dirs().sort_stats(-1).print_stats()

