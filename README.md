# qnaElm
A REST Elm Client to consume Q and A services from Microsoft 

An improvement over the stock example found in the elm-lang/try list. 
And also connecting to Microsoft's cognitive services for Q&A. 

# Version 0.x
![version1](https://github.com/kgashok/qnaElm/blob/master/img/qnaService.png)

# Version 1.x
![version2](https://github.com/kgashok/qnaElm/blob/master/img/version2.png)

For a demo, click [this](https://preview.c9users.io/kgashok/qnamicro/index.html?_c9_id=livepreview1&_c9_host=https://ide.c9.io).

## Important Reading 

JSON parsing is a minefield https://news.ycombinator.com/item?id=12796556


### Resolving Issue #19 

The moment to do this is after you have received and 
processed your last HTTP response. So in a branch in your `update`, 
something like

```
case msg of
    ReceivedHttpResponse response ->
        let
            newModel = processInModel response model
        in
            if (hasLastResponse newModel) then
                 update SortList newModel
            else
                  (newModel, Cmd.none)
```
