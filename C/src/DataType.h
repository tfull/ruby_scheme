#ifndef __DATA_TYPE_H__
#define __DATA_TYPE_H__

typedef struct{
    void *data;
}Token;

typedef struct{
    Token *tokens;
    String storage;
    State state;
}Buffer;

#endif
