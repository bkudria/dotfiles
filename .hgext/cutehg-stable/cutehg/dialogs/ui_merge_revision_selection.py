# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/merge_revision_selection.ui'
#
# Created: Wed Jun  3 10:23:38 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_mergeRevisionSelection(object):
    def setupUi(self, mergeRevisionSelection):
        mergeRevisionSelection.setObjectName("mergeRevisionSelection")
        mergeRevisionSelection.resize(554, 253)
        self.verticalLayout = QtGui.QVBoxLayout(mergeRevisionSelection)
        self.verticalLayout.setObjectName("verticalLayout")
        self.historyChoiceLabel = QtGui.QLabel(mergeRevisionSelection)
        self.historyChoiceLabel.setWordWrap(True)
        self.historyChoiceLabel.setObjectName("historyChoiceLabel")
        self.verticalLayout.addWidget(self.historyChoiceLabel)
        self.widget_2 = QtGui.QWidget(mergeRevisionSelection)
        self.widget_2.setObjectName("widget_2")
        self.horizontalLayout_2 = QtGui.QHBoxLayout(self.widget_2)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.findLineEdit = QtGui.QLineEdit(self.widget_2)
        self.findLineEdit.setObjectName("findLineEdit")
        self.horizontalLayout_2.addWidget(self.findLineEdit)
        self.fieldComboBox = QtGui.QComboBox(self.widget_2)
        self.fieldComboBox.setObjectName("fieldComboBox")
        self.fieldComboBox.addItem(QtCore.QString())
        self.horizontalLayout_2.addWidget(self.fieldComboBox)
        self.findButton = QtGui.QPushButton(self.widget_2)
        self.findButton.setObjectName("findButton")
        self.horizontalLayout_2.addWidget(self.findButton)
        self.verticalLayout.addWidget(self.widget_2)
        self.historyView = QtGui.QTableView(mergeRevisionSelection)
        self.historyView.setObjectName("historyView")
        self.verticalLayout.addWidget(self.historyView)

        self.retranslateUi(mergeRevisionSelection)
        QtCore.QMetaObject.connectSlotsByName(mergeRevisionSelection)

    def retranslateUi(self, mergeRevisionSelection):
        mergeRevisionSelection.setWindowTitle(QtGui.QApplication.translate("mergeRevisionSelection", "Form", None, QtGui.QApplication.UnicodeUTF8))
        self.historyChoiceLabel.setText(QtGui.QApplication.translate("mergeRevisionSelection", "Choose a revision to merge the current working directory with.", None, QtGui.QApplication.UnicodeUTF8))
        self.fieldComboBox.setItemText(0, QtGui.QApplication.translate("mergeRevisionSelection", "All Fields", None, QtGui.QApplication.UnicodeUTF8))
        self.findButton.setText(QtGui.QApplication.translate("mergeRevisionSelection", "Find", None, QtGui.QApplication.UnicodeUTF8))

