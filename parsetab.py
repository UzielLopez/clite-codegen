
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = "ID INTLIT\n    Addition : Term\n             | Addition AddOp Term\n    \n    Term : Term MulOp Factor\n         | Factor\n    \n    AddOp : '+'\n          | '-'\n    \n    MulOp : '*'\n          | '/'\n          | '%'\n    \n    Factor : Primary\n    \n    Primary : INTLIT \n            | '(' Addition ')'\n    "
    
_lr_action_items = {'INTLIT':([0,6,7,8,9,10,11,12,13,],[5,5,5,-5,-6,5,-7,-8,-9,]),'(':([0,6,7,8,9,10,11,12,13,],[6,6,6,-5,-6,6,-7,-8,-9,]),'$end':([1,2,3,4,5,15,16,17,],[0,-1,-4,-10,-11,-2,-3,-12,]),'+':([1,2,3,4,5,14,15,16,17,],[8,-1,-4,-10,-11,8,-2,-3,-12,]),'-':([1,2,3,4,5,14,15,16,17,],[9,-1,-4,-10,-11,9,-2,-3,-12,]),')':([2,3,4,5,14,15,16,17,],[-1,-4,-10,-11,17,-2,-3,-12,]),'*':([2,3,4,5,15,16,17,],[11,-4,-10,-11,11,-3,-12,]),'/':([2,3,4,5,15,16,17,],[12,-4,-10,-11,12,-3,-12,]),'%':([2,3,4,5,15,16,17,],[13,-4,-10,-11,13,-3,-12,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'Addition':([0,6,],[1,14,]),'Term':([0,6,7,],[2,2,15,]),'Factor':([0,6,7,10,],[3,3,3,16,]),'Primary':([0,6,7,10,],[4,4,4,4,]),'AddOp':([1,14,],[7,7,]),'MulOp':([2,15,],[10,10,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> Addition","S'",1,None,None,None),
  ('Addition -> Term','Addition',1,'p_Addition','analisis.py',32),
  ('Addition -> Addition AddOp Term','Addition',3,'p_Addition','analisis.py',33),
  ('Term -> Term MulOp Factor','Term',3,'p_Term','analisis.py',42),
  ('Term -> Factor','Term',1,'p_Term','analisis.py',43),
  ('AddOp -> +','AddOp',1,'p_AddOp','analisis.py',52),
  ('AddOp -> -','AddOp',1,'p_AddOp','analisis.py',53),
  ('MulOp -> *','MulOp',1,'p_MulOp','analisis.py',61),
  ('MulOp -> /','MulOp',1,'p_MulOp','analisis.py',62),
  ('MulOp -> %','MulOp',1,'p_MulOp','analisis.py',63),
  ('Factor -> Primary','Factor',1,'p_Factor','analisis.py',69),
  ('Primary -> INTLIT','Primary',1,'p_Primary','analisis.py',75),
  ('Primary -> ( Addition )','Primary',3,'p_Primary','analisis.py',76),
]
