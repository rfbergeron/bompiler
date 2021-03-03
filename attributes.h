#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__

// attributes correspond to array indices in the order they are listed here
enum attr {
    ATTR_VOID,
    ATTR_INT,
    ATTR_NULL,
    ATTR_STRING,
    ATTR_STRUCT,
    ATTR_ARRAY,
    ATTR_FUNCTION,
    ATTR_VARIABLE,
    ATTR_FIELD,
    ATTR_TYPEID,
    ATTR_PARAM,
    ATTR_LOCAL,
    ATTR_LVAL,
    ATTR_CONST,
    ATTR_VREG,
    ATTR_VADDR,
    NUM_ATTRIBUTES
};

extern char attr_map[][32];

#endif
