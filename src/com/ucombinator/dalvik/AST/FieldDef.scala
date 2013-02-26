package com.ucombinator.dalvik.AST

class FieldDef(name: String, 
    typ: String, 
    visibility: VisibilityAttr, 
    isStatic: Boolean, 
    isFinal: Boolean) {

}

// TODO: need to get Field == FieldDef
class Field(var classType:JavaType, var fieldType:JavaType, val name:String)

