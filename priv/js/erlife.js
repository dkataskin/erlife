(function(){
    var erlife = window.erlife = {
        isRunning: false,

        onUpdate: function(genNum, nodeCount){
        },

        onSavedStatesListLoaded: function(states){
        },

        server: {
            bullet: null,

            init: function(address, sessionId){
                var self = this;
                this.bullet = $.bullet(address);

                this.bullet.onopen = function(){
                    console.log('bullet: opened');
                };

                this.bullet.ondisconnect = function(){
                    console.log('bullet: disconnected');
                };

                this.bullet.onclose = function(){
                    console.log('bullet: closed');
                };

                this.bullet.onmessage = function(e){
                    if (e.data == "pong"){
                        console.log("pong");
                    }
                    else {
                        var event = $.parseJSON(e.data);
                        if (event){
                            if (event.event == "nextGen"){
                                var data = event.data;
                                erlife.eventHandlers.onNextGen(data.num, data.nodeCount, data.delta);
                            } else if (event.event == "savedstates"){
                                var data = event.data;
                                erlife.onSavedStatesListLoaded(data);
                                console.log("saved states loaded " + data);
                            }
                        }
                    }
                };

                this.bullet.onheartbeat = function(){
                    self.bullet.send('ping: ' + sessionId);
                }
            },

            nextGen: function(viewport, changes, invalidate){
                this.bullet.send($.toJSON({ command: "nextGen",
                                            data: { viewport: [viewport.minX, viewport.minY, viewport.maxX, viewport.maxY],
                                                    statechanges: changes,
                                                    invalidate: invalidate
                                                  }
                                          }));
            },

            clear: function(){
                this.bullet.send($.toJSON({ command: "clear" }));
            },

            saveState: function(name, changes){
                this.bullet.send($.toJSON({ command: "save",
                                            data: { name: name,
                                                    statechanges: changes,
                                            }}));
            },

            loadState: function(id){
                this.bullet.send($.toJSON({ command: "load",
                                            data: {
                                                    id: id,
                                                    viewport: [viewport.minX, viewport.minY, viewport.maxX, viewport.maxY]
                                                  }}));
            }
        },

        eventHandlers: {
            onNextGen: function(genNum, nodeCount, delta){
                erlife.canvas.drawDelta(delta);
                erlife.onUpdate(genNum, nodeCount);

                setTimeout(function() {
                    if (erlife.isRunning){
                        erlife.update([]);
                    }}, 100);
            }
        },

        canvas: {
            context: null,
            canvas: null,
            width: 900,
            height: 500,
            cols: 99,
            rows: 99,
            offsetX: 45,
            offsetY: 45,
            cellSize: 8,
            cellSpace: 1,
            backgroundColor: '#B5B8B4',
            liveCellColor: '#28D18D',
            emptyCellColor: '#F3F3F3',
            mousedown: false,
            dragging: false,
            userState: {
                array: [],

                set: function(x, y){
                    var exists = false;
                    var index = 0;
                    for(i = 0; i < this.array.length; i++){
                        if (this.array[i].x == x && this.array[i].y == y){
                            exists = true;
                            index = i;
                            break;
                        }
                    }

                    if (exists){
                        this.array.pop(index);
                    }
                    else {
                        this.array.push({x: x, y: y});
                    }

                    return !exists;
                },

                clear: function(){
                    this.array = [];
                },

                getState: function(){
                    var result = new Array();
                    for(var i = 0; i < this.array.length; i++){
                        var node = this.array[i];
                        result.push(node.x - erlife.canvas.offsetX + 1);
                        result.push(node.y - erlife.canvas.offsetY + 1);
                        result.push(true);
                    }

                    return result;
                }
            },

            init: function(canvas, context){
                this.canvas = canvas;
                this.context = context;
                this.canvas.onmousedown = this.onmousedown;
                this.canvas.onmouseup = this.onmouseup;
                this.canvas.onmousemove = this.onmousemove;

                this.draw();
            },

            clear: function(){
                erlife.canvas.draw();
                erlife.canvas.userState.clear();
            },

            draw: function(){
                // Dynamic canvas size
                this.width = (this.cellSpace * this.cols) + (this.cellSize * this.cols);
                this.canvas.setAttribute('width', this.width);

                this.height = (this.cellSpace * this.rows) + (this.cellSize * this.rows);
                this.canvas.getAttribute('height', this.height);

                // Fill background
                this.context.fillStyle = this.backgroundColor;
                console.log(this.context.fillStyle);
                this.context.fillRect(0, 0, this.width, this.height);

                for (i = 0; i < this.cols; i++) {
                  for (j = 0; j < this.rows; j++) {
                    this.drawCell(i, j, false);
                  }
                }
            },

            drawDelta: function(delta){
                var self = this;
                delta.forEach(function(array) {
                    var x = array[1] + self.offsetX - 1;
                    var y = array[2] + self.offsetY - 1;
                    self.drawCell(x, y, array[0]);
                });

                for(i = 0; i++; i < delta.length){
                    console.log("i " + i);
                    // [false | true, X, Y]
                    var x = delta[i][1] + this.canvas.offsetX - 1;
                    var y = delta[i][2] + this.canvas.offsetY - 1;
                    console.log("x " + x + " y " + y);
                    this.drawCell(x, y, delta[i][0]);
                }
            },

            drawCell: function (i, j, alive) {
              if (alive) {
                this.context.fillStyle = this.liveCellColor;
              } else {
                this.context.fillStyle = this.emptyCellColor;
              }

              this.context.fillRect(this.cellSpace + (this.cellSpace * i) + (this.cellSize * i),
                                    this.cellSpace + (this.cellSpace * j) + (this.cellSize * j),
                                    this.cellSize,
                                    this.cellSize);
            },

            mousePosition: function(e){
                // http://www.malleus.de/FAQ/getImgMousePos.html
                // http://www.quirksmode.org/js/events_properties.html#position
                var event, x, y, domObject, posx = 0, posy = 0, top = 0, left = 0, cellSize = this.cellSize + 1;

                event = e;
                if (!event) {
                  event = window.event;
                }

                if (event.pageX || event.pageY){
                  posx = event.pageX;
                  posy = event.pageY;
                } else if (event.clientX || event.clientY){
                    posx = event.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
                    posy = event.clientY + document.body.scrollTop + document.documentElement.scrollTop;
                }

                domObject = event.target || event.srcElement;

                while ( domObject.offsetParent ) {
                  left += domObject.offsetLeft;
                  top += domObject.offsetTop;
                  domObject = domObject.offsetParent;
                }

                domObject.pageTop = top;
                domObject.pageLeft = left;

                x = Math.ceil(((posx - domObject.pageLeft)/cellSize) - 1);
                y = Math.ceil(((posy - domObject.pageTop)/cellSize) - 1);

                console.log("x=" + x + " y=" + y);
                return {x: x, y: y};
            },

            onmousedown: function(e){
                erlife.canvas.mousedown = true;
            },

            onmousemove: function(e){
                if (erlife.canvas.mousedown && !erlife.canvas.dragging){
                    erlife.canvas.dragging = true;
                }
            },

            onmouseup: function(e){
                erlife.canvas.mousedown = false;
                if (erlife.canvas.dragging){
                    erlife.canvas.dragging = false;
                } else {
                    var position = erlife.canvas.mousePosition(e);
                    if (erlife.canvas.userState.set(position.x, position.y)){
                        erlife.canvas.drawCell(position.x, position.y, true);
                    }
                    else{
                        erlife.canvas.drawCell(position.x, position.y, false);
                    }
                }
            }
        },

        viewport: {
            offsetX: 0,
            offsetY: 0,
            viewPortWidth: 100,
            viewPortHeight: 100,
            invalidate: false,

            setOffset: function(offsetX, offsetY){
                this.offsetX = offsetX;
                this.offsetY = offsetY;
            },

            getView: function(){
                return {
                    minX: erlife.viewport.offsetX - (erlife.viewport.viewPortWidth / 2),
                    maxX: erlife.viewport.offsetX + (erlife.viewport.viewPortWidth / 2),
                    minY: erlife.viewport.offsetY - (erlife.viewport.viewPortHeight / 2),
                    maxY: erlife.viewport.offsetY + (erlife.viewport.viewPortHeight / 2)
                }
            }
        },

        run: function(){
            if (!this.isRunning){
                var changesToState = this.canvas.userState.getState();
                this.canvas.userState.clear();

                this.isRunning = true;
                this.update(changesToState);
            }
        },

        pause: function(){
            if (this.isRunning){
                this.isRunning = false;
            }
        },

        nextGen: function(){
            if (!this.isRunning){
                var changesToState = this.canvas.userState.getState();
                this.canvas.userState.clear();
                this.update(changesToState);
            }
        },

        clear: function(){
            this.isRunning = false;
            this.server.clear();
            this.canvas.clear();

            this.onUpdate(0, 0);
        },

        update: function(changes){
            var viewport = this.viewport.getView();
            this.server.nextGen(viewport, changes, this.viewport.invalidate);
        },

        saveState: function(name){
            var changesToState = this.canvas.userState.getState();
            this.canvas.userState.clear();
            this.server.saveState(name, changesToState);
        },

        loadState: function(id){
            if (!this.isRunning){
                this.server.loadState(id);
            }
        },

        init: function(config){
            this.canvas.init(config.canvas, config.context);
            this.server.init(config.address, config.sessionId);
        }
    };
})()