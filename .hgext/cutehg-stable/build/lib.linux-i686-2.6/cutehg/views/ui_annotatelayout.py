# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/views/layouts/annotatelayout.ui'
#
# Created: Wed Jun  3 10:23:39 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_AnnotateLayout(object):
    def setupUi(self, AnnotateLayout):
        AnnotateLayout.setObjectName("AnnotateLayout")
        AnnotateLayout.resize(590, 461)
        self.verticalLayout = QtGui.QVBoxLayout(AnnotateLayout)
        self.verticalLayout.setObjectName("verticalLayout")
        self.widget = QtGui.QWidget(AnnotateLayout)
        self.widget.setObjectName("widget")
        self.horizontalLayout = QtGui.QHBoxLayout(self.widget)
        self.horizontalLayout.setMargin(0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtGui.QLabel(self.widget)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        self.revision = QtGui.QLineEdit(self.widget)
        self.revision.setEnabled(False)
        self.revision.setObjectName("revision")
        self.horizontalLayout.addWidget(self.revision)
        self.verticalLayout.addWidget(self.widget)
        self.tableView = QtGui.QTableView(AnnotateLayout)
        self.tableView.setMaximumSize(QtCore.QSize(16777215, 16777215))
        font = QtGui.QFont()
        font.setFamily("Courier New")
        self.tableView.setFont(font)
        self.tableView.setContextMenuPolicy(QtCore.Qt.ActionsContextMenu)
        self.tableView.setStyleSheet("padding: 0;")
        self.tableView.setAutoScrollMargin(16)
        self.tableView.setAlternatingRowColors(False)
        self.tableView.setSelectionMode(QtGui.QAbstractItemView.SingleSelection)
        self.tableView.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.tableView.setShowGrid(False)
        self.tableView.setGridStyle(QtCore.Qt.NoPen)
        self.tableView.setWordWrap(False)
        self.tableView.setObjectName("tableView")
        self.verticalLayout.addWidget(self.tableView)
        self.status = QtGui.QTextEdit(AnnotateLayout)
        self.status.setEnabled(True)
        self.status.setMaximumSize(QtCore.QSize(16777215, 125))
        self.status.setReadOnly(True)
        self.status.setObjectName("status")
        self.verticalLayout.addWidget(self.status)
        self.actionGotoRev = QtGui.QAction(AnnotateLayout)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/svn_switch.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionGotoRev.setIcon(icon)
        self.actionGotoRev.setObjectName("actionGotoRev")

        self.retranslateUi(AnnotateLayout)
        QtCore.QMetaObject.connectSlotsByName(AnnotateLayout)
        AnnotateLayout.setTabOrder(self.revision, self.tableView)

    def retranslateUi(self, AnnotateLayout):
        AnnotateLayout.setWindowTitle(QtGui.QApplication.translate("AnnotateLayout", "AnnotateLayout", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("AnnotateLayout", "Revision:", None, QtGui.QApplication.UnicodeUTF8))
        self.actionGotoRev.setText(QtGui.QApplication.translate("AnnotateLayout", "Annotate from before this change", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
