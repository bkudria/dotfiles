#! /usr/bin/env python

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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  US

from PyQt4 import QtCore, QtGui
import qtutil

class CommitMessageView(QtGui.QWidget):
    def __init__(self, text='', parent=None):
        QtGui.QWidget.__init__(self, parent)
        
        self._previous = QtGui.QComboBox()
        self._text = QtGui.QPlainTextEdit()
        self._position = QtGui.QLabel()

        self._setupPrevious()
        self._setupText(text)
        self._setupPosition()

        self._layout = QtGui.QVBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.addWidget(self._previous)
        self._layout.addWidget(self._text)
        self._layout.addWidget(self._position)
        self.setLayout(self._layout)

    def ensureWidth(self, size):
        metrics = self._text.fontMetrics()
        self.setMinimumWidth(metrics.width((size+1)*'w'))

    def value(self):
        return str(self._text.toPlainText().toUtf8())

    def close(self):
        self._saveToPrevious()
        return QtGui.QWidget.close(self)

    def _setupPrevious(self):
        pass

    def _saveToPrevious(self):
        pass

    def _setupText(self, text):
        w = self._text
        w.setPlainText(text)
        
        f = qtutil.getFixedFont()
        if f:
            w.setFont(f)

        w.setSizePolicy(QtGui.QSizePolicy.MinimumExpanding,
                QtGui.QSizePolicy.MinimumExpanding)
        assert self.connect(w, QtCore.SIGNAL('cursorPositionChanged()'),
                self._updatePosition)

    def _setupPosition(self):
        w = self._position
        w.setIndent(6)
        w.setAlignment(QtCore.Qt.AlignRight)
        self._updatePosition()

    def _updatePosition(self):
        t = self.value()
        cursor = self._text.textCursor()
        
        start = cursor.selectionStart()
        end = cursor.selectionEnd()

        def coord(ix):
            lines = t[:ix].split('\n')
            return "%d,%d" % (len(lines), len(lines[-1]) + 1)

        p = ""
        if start != end:
            p = coord(start) + " - "
        self._position.setText(p + coord(end))

if __name__ == '__main__':

    class dlg(QtGui.QDialog):
        def __init__(self):
            QtGui.QDialog.__init__(self)
            l = QtGui.QVBoxLayout(self)
            l.addWidget(CommitMessageView("Hi all"))
            self.setLayout(l)


    import sys
    app = QtGui.QApplication( sys.argv )
    dialog = dlg()
    dialog.show()
    sys.exit(app.exec_())

