# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/views/layouts/historylayout.ui'
#
# Created: Wed Jun  3 10:23:39 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_HistoryLayout(object):
    def setupUi(self, HistoryLayout):
        HistoryLayout.setObjectName("HistoryLayout")
        HistoryLayout.resize(589, 467)
        self.verticalLayout = QtGui.QVBoxLayout(HistoryLayout)
        self.verticalLayout.setObjectName("verticalLayout")
        self.widget = QtGui.QWidget(HistoryLayout)
        self.widget.setObjectName("widget")
        self.horizontalLayout = QtGui.QHBoxLayout(self.widget)
        self.horizontalLayout.setMargin(0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.findText = QtGui.QLineEdit(self.widget)
        self.findText.setObjectName("findText")
        self.horizontalLayout.addWidget(self.findText)
        self.findFieldsCombo = QtGui.QComboBox(self.widget)
        self.findFieldsCombo.setObjectName("findFieldsCombo")
        self.findFieldsCombo.addItem(QtCore.QString())
        self.horizontalLayout.addWidget(self.findFieldsCombo)
        self.findPreviousButton = QtGui.QPushButton(self.widget)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/go-up-search.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.findPreviousButton.setIcon(icon)
        self.findPreviousButton.setObjectName("findPreviousButton")
        self.horizontalLayout.addWidget(self.findPreviousButton)
        self.findNextButton = QtGui.QPushButton(self.widget)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/go-down-search.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.findNextButton.setIcon(icon1)
        self.findNextButton.setObjectName("findNextButton")
        self.horizontalLayout.addWidget(self.findNextButton)
        self.verticalLayout.addWidget(self.widget)
        self.tableView = QtGui.QTableView(HistoryLayout)
        self.tableView.setAlternatingRowColors(False)
        self.tableView.setSelectionMode(QtGui.QAbstractItemView.SingleSelection)
        self.tableView.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.tableView.setShowGrid(False)
        self.tableView.setGridStyle(QtCore.Qt.NoPen)
        self.tableView.setObjectName("tableView")
        self.verticalLayout.addWidget(self.tableView)

        self.retranslateUi(HistoryLayout)
        QtCore.QMetaObject.connectSlotsByName(HistoryLayout)

    def retranslateUi(self, HistoryLayout):
        HistoryLayout.setWindowTitle(QtGui.QApplication.translate("HistoryLayout", "HistoryLayout", None, QtGui.QApplication.UnicodeUTF8))
        self.findFieldsCombo.setItemText(0, QtGui.QApplication.translate("HistoryLayout", "All Fields", None, QtGui.QApplication.UnicodeUTF8))
        self.findPreviousButton.setText(QtGui.QApplication.translate("HistoryLayout", "Previous", None, QtGui.QApplication.UnicodeUTF8))
        self.findNextButton.setText(QtGui.QApplication.translate("HistoryLayout", "Next", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
