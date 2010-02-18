# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cutehg/ui/update.ui'
#
# Created: Wed Jun  3 10:23:39 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_UpdateDialog(object):
    def setupUi(self, UpdateDialog):
        UpdateDialog.setObjectName("UpdateDialog")
        UpdateDialog.resize(627, 436)
        self.verticalLayout = QtGui.QVBoxLayout(UpdateDialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.buttonContainer = QtGui.QWidget(UpdateDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Maximum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.buttonContainer.sizePolicy().hasHeightForWidth())
        self.buttonContainer.setSizePolicy(sizePolicy)
        self.buttonContainer.setObjectName("buttonContainer")
        self.horizontalLayout = QtGui.QHBoxLayout(self.buttonContainer)
        self.horizontalLayout.setObjectName("horizontalLayout")
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.cancelButton = QtGui.QPushButton(self.buttonContainer)
        self.cancelButton.setObjectName("cancelButton")
        self.horizontalLayout.addWidget(self.cancelButton)
        spacerItem1 = QtGui.QSpacerItem(15, 20, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.updateButton = QtGui.QPushButton(self.buttonContainer)
        self.updateButton.setObjectName("updateButton")
        self.horizontalLayout.addWidget(self.updateButton)
        self.verticalLayout.addWidget(self.buttonContainer)
        self.actionUpdate = QtGui.QAction(UpdateDialog)
        self.actionUpdate.setObjectName("actionUpdate")
        self.actionCancel = QtGui.QAction(UpdateDialog)
        self.actionCancel.setObjectName("actionCancel")
        self.actionFind = QtGui.QAction(UpdateDialog)
        self.actionFind.setObjectName("actionFind")

        self.retranslateUi(UpdateDialog)
        QtCore.QObject.connect(self.updateButton, QtCore.SIGNAL("clicked()"), self.actionUpdate.trigger)
        QtCore.QObject.connect(self.cancelButton, QtCore.SIGNAL("clicked()"), self.actionCancel.trigger)
        QtCore.QMetaObject.connectSlotsByName(UpdateDialog)

    def retranslateUi(self, UpdateDialog):
        UpdateDialog.setWindowTitle(QtGui.QApplication.translate("UpdateDialog", "Mercurial Update", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setToolTip(QtGui.QApplication.translate("UpdateDialog", "Cancel updating.\n"
"\n"
"Esc for a shortcut.", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setText(QtGui.QApplication.translate("UpdateDialog", "Close", None, QtGui.QApplication.UnicodeUTF8))
        self.cancelButton.setShortcut(QtGui.QApplication.translate("UpdateDialog", "Esc", None, QtGui.QApplication.UnicodeUTF8))
        self.updateButton.setToolTip(QtGui.QApplication.translate("UpdateDialog", "Update to the selected revision.\n"
"\n"
"Use Ctrl+Enter as a shortcut. ", None, QtGui.QApplication.UnicodeUTF8))
        self.updateButton.setText(QtGui.QApplication.translate("UpdateDialog", "Update", None, QtGui.QApplication.UnicodeUTF8))
        self.updateButton.setShortcut(QtGui.QApplication.translate("UpdateDialog", "Ctrl+Return", None, QtGui.QApplication.UnicodeUTF8))
        self.actionUpdate.setText(QtGui.QApplication.translate("UpdateDialog", "Update", None, QtGui.QApplication.UnicodeUTF8))
        self.actionCancel.setText(QtGui.QApplication.translate("UpdateDialog", "Cancel", None, QtGui.QApplication.UnicodeUTF8))
        self.actionFind.setText(QtGui.QApplication.translate("UpdateDialog", "Find", None, QtGui.QApplication.UnicodeUTF8))

