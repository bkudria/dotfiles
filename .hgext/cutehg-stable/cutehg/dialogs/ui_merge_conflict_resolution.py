# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/merge_conflict_resolution.ui'
#
# Created: Wed Jun  3 10:23:38 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_mergeConflictResolution(object):
    def setupUi(self, mergeConflictResolution):
        mergeConflictResolution.setObjectName("mergeConflictResolution")
        mergeConflictResolution.resize(573, 362)
        self.verticalLayout = QtGui.QVBoxLayout(mergeConflictResolution)
        self.verticalLayout.setObjectName("verticalLayout")
        self.mergeConflictLabel = QtGui.QLabel(mergeConflictResolution)
        self.mergeConflictLabel.setWordWrap(True)
        self.mergeConflictLabel.setObjectName("mergeConflictLabel")
        self.verticalLayout.addWidget(self.mergeConflictLabel)
        self.conflictView = QtGui.QTableView(mergeConflictResolution)
        self.conflictView.setObjectName("conflictView")
        self.verticalLayout.addWidget(self.conflictView)

        self.retranslateUi(mergeConflictResolution)
        QtCore.QMetaObject.connectSlotsByName(mergeConflictResolution)

    def retranslateUi(self, mergeConflictResolution):
        mergeConflictResolution.setWindowTitle(QtGui.QApplication.translate("mergeConflictResolution", "Form", None, QtGui.QApplication.UnicodeUTF8))
        self.mergeConflictLabel.setText(QtGui.QApplication.translate("mergeConflictResolution", "There were conflicts. Please resolve them and click the checkbox to signify you\'ve resolved each conflicting file.", None, QtGui.QApplication.UnicodeUTF8))

