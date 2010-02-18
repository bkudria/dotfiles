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

from historymodel import HistoryModel
import colorutil
from PyQt4 import QtCore, QtGui

def createWorkingPolygon():
    polygon = QtGui.QPolygon()
    points = [ 0, 0,
               2, -2,
               4, 0,
               3, 0,
               3, 2,
               1, 2,
               1, 0
               ]
    polygon.setPoints(points)
    return polygon


def createTagPolygon(font, text):
    ''' create an empty polygon that will fit the text 
    with the given font using the tag shape and color
    '''

    fontMetrics = QtGui.QFontMetrics(font)
    textRect = fontMetrics.boundingRect(text)

    width = textRect.width() + 6
    height = textRect.height() + 6

    polygon = QtGui.QPolygon()
    points = [ 4, 0, 
              0, height/2, 
              4, height,
              width + 4, height,
              width,     height/2,
              width + 4, 0
            ]
    polygon.setPoints(points)

    return polygon


class HistoryGraphDelegate(QtGui.QStyledItemDelegate):
    def __init__(self):
        QtGui.QStyledItemDelegate.__init__(self)
        self.COLUMN_WIDTH = 9 # number of pixels each column takes up
        self.colors = []
        self.max_color = 0
        
    def init_colors(self, fg, bg, max_color):
        self.max_color = min(max_color + 1, 32)
        if len(self.colors) != self.max_color:
            self.colors = colorutil.colorset(fg, bg, self.max_color)

        while len(self.colors) < max_color +1:
            self.colors.extend(self.colors)
            

    def paint(self, painter, option, index):
        QtGui.QStyledItemDelegate.paint(self, painter, option, index)

        if index.model().columns.getName(index.column()) != "Graph":
            return 
        
        font = QtGui.QApplication.font()
        palette = QtGui.QApplication.palette()


        (column, color, edges, pedges, max_columns, max_color) = index.model().data(index, HistoryModel.GraphRole)
        self.init_colors(palette.color(QtGui.QPalette.Text), palette.color(QtGui.QPalette.Base), max_color)
        item = index.model().data(index, HistoryModel.RevisionRole)

        # setup painter
        painter.setRenderHint(QtGui.QPainter.Antialiasing, True)
        painter.setClipRect(option.rect.x(), option.rect.y(), option.rect.width(), option.rect.height())
        if pedges is None:
            pedges = []


        currentX = option.rect.x() + 3
        currentY = option.rect.y() + 3

        # draw each incoming line drawing downwards from the center of the previous cell 
        for (col, next_col, edge_color) in pedges:
            beginY = option.rect.y()  - option.rect.height()/2
            endY = option.rect.y() + option.rect.height()/2 
            beginX = option.rect.x() + col*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2
            endX = beginX 
            
            if next_col != col:
                endX = option.rect.x() + next_col*self.COLUMN_WIDTH  + self.COLUMN_WIDTH/2

            qcolor = self.colors[edge_color]
            painter.setPen(qcolor)
            painter.setBrush(qcolor)
            path = QtGui.QPainterPath()
            path.moveTo(beginX, beginY)
            path.cubicTo(QtCore.QPointF(beginX, beginY + (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY - (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY))
            painter.strokePath( path, painter.pen() )
   
        # draw each outgoing line drawing downwards from the middle of the cell to the middle of the next cell (one below)
        for (col, next_col, edge_color) in edges:
            beginY = option.rect.y() + option.rect.height()/2 + 1
            endY = option.rect.y() + option.rect.height() + option.rect.height()/2
            beginX = option.rect.x() + col*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2
            endX = beginX 
            
            if next_col != col:
                endX = option.rect.x() + next_col*self.COLUMN_WIDTH  + self.COLUMN_WIDTH/2

            qcolor = self.colors[edge_color]
            painter.setPen(qcolor)
            painter.setBrush(qcolor)
            path = QtGui.QPainterPath()
            path.moveTo(beginX, beginY)
            path.cubicTo(QtCore.QPointF(beginX, beginY + (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY - (endY-beginY)/2.0),
                         QtCore.QPointF(endX, endY))
            painter.strokePath( path, painter.pen() )

        # Draw the node for the cell
        qcolor = self.colors[color]
        painter.setPen(qcolor)
        painter.setBrush(qcolor)
        painter.drawEllipse( QtCore.QPointF( option.rect.x() + column*self.COLUMN_WIDTH + self.COLUMN_WIDTH/2,
                                             option.rect.y() + option.rect.height()/2 ),
                             3, 3 )


    def sizeHint(self, option, index):
        if index.isValid() and index.model().columns.getName(index.column()) != "Graph":
            return 

        (column, color, edges, pedges, max_columns, max_color) = index.model().data(index, HistoryModel.GraphRole)

        item = index.model().data(index, HistoryModel.RevisionRole)

        return QtCore.QSize(max_columns*self.COLUMN_WIDTH, 15)

