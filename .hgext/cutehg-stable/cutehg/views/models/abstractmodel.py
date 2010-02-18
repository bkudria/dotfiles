#!/usr/bin/env python

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

from PyQt4 import QtCore, QtGui
from mercurial import templatefilters

class TableColumns(object):
    def __init__(self):
        self.__nameToIndex = dict() 
        self.__indexToName = []

    def addColumn(self, name):
        self.__indexToName.append( name )
        self.__nameToIndex[name] = len(self.__indexToName) - 1 

    def getName(self, index):
        return self.__indexToName[index]

    def getIndex(self, name):
        return self.__nameToIndex[name]

    def getNumberOfColumns(self):
        return len(self.__indexToName)

class AbstractItem(object):
    def __init__(self, context):
        self.context = context
        
        self.revid = context.rev()
        self.author = QtCore.QString.fromUtf8(templatefilters.person(context.user()))
        self.fullmessage = QtCore.QString.fromUtf8(context.description())
        self.message = QtCore.QString.fromUtf8(self.fullmessage.split('\n')[0])
        self.age = templatefilters.age(context.date()) 
        self.timestamp = context.date()
        if hasattr(context, 'tags'):
            try:
                self.tags = context.tags()
            except:
                self.tags = []
        self.branch = context.branch()

