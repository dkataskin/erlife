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
                        //console.log("pong");
                    }
                    else {
                        var event = $.parseJSON(e.data);
                        if (event){
                            var data = event.data;
                            if (event.event == "nextGen"){
                                erlife.eventHandlers.onNextGen(data.num, data.nodeCount, data.delta);
                            } else if (event.event == "savedstates"){
                                erlife.onSavedStatesListLoaded(data);
                            } else if (event.event == "viewport"){
                                erlife.eventHandlers.onViewport(data);
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

            getViewPort: function(viewport){
                this.bullet.send($.toJSON({ command: "viewport",
                                            data: [viewport.minX, viewport.minY, viewport.maxX, viewport.maxY]
                                          }));
            },

            clear: function(){
                this.bullet.send($.toJSON({ command: "clear" }));
            },

            saveState: function(id, name, changes){
                this.bullet.send($.toJSON({ command: "save",
                                            data: { id: id,
                                                    name: name,
                                                    statechanges: changes,
                                            }}));
            },

            loadState: function(id, viewport){
                this.bullet.send($.toJSON({ command: "load",
                                            data: { id: id,
                                                    viewport: [viewport.minX, viewport.minY, viewport.maxX, viewport.maxY]
                                                  }}));
            }
        },

        eventHandlers: {
            onNextGen: function(genNum, nodeCount, delta){
                erlife.canvas.applyDelta(delta);
                erlife.onUpdate(genNum, nodeCount);
                erlife.viewport.invalidate = false;

                setTimeout(function() {
                    if (erlife.isRunning){
                        erlife.update([]);
                    }}, 50);
            },

            onViewport: function(viewportData){
                console.log("refresh viewport");
                erlife.viewport.invalidate = false;
                erlife.canvas.clear();
                erlife.canvas.applyDelta(viewportData);
            }
        },

        canvas: {
            context: null,
            canvas: null,
            width: 900,
            height: 500,
            cols: 99,
            rows: 99,
            offsetX: 49,
            offsetY: 49,
            cellSize: 8,
            cellSpace: 1,
            backgroundColor: '#B5B8B4',
            liveCellColor: '#28D18D',
            emptyCellColor: '#F3F3F3',
            invalidateCellColor: '#B25ED3',
            mousedown: false,
            lastX: -1,
            lastY: -1,
            state: null,
            userState: {
                array: [],

                set: function(x, y){
                    var cellValue = erlife.canvas.state[x][y];
                    cellValue = !cellValue;
                    erlife.canvas.state[x][y] = cellValue;

                    erlife.canvas.drawCell(x, y, cellValue);

                    var exists = false;
                    var index = 0;
                    for(var i = 0; i < this.array.length; i++){
                        if (this.array[i].x == x && this.array[i].y == y){
                            exists = true;
                            index = i;
                            break;
                        }
                    }

                    if (exists){
                        this.array.pop(index);
                    }

                    this.array.push({action: cellValue, x: x, y: y});
                },

                clear: function(){
                    this.array = [];
                },

                getState: function(){
                    var result = new Array();
                    for(var i = 0; i < this.array.length; i++){
                        var node = this.array[i];
                        var x = node.x - erlife.canvas.offsetX + 1;
                        var y = node.y - erlife.canvas.offsetY + 1;
                        var point = erlife.viewport.translateToServer(x, y);
                        result.push(point[0]);
                        result.push(point[1]);
                        result.push(node.action);
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
                this.canvas.onselectstart = function () { return false; };

                this.state = new Array();
                for(var i = 0; i < this.cols; i++){
                    var row = new Array();
                    for(var j = 0; j < this.rows; j++){
                        row.push(false);
                    }

                    this.state.push(row);
                };

                this.draw();
            },

            clear: function(){
                this.draw();
                this.userState.clear();

                for(var i = 0; i < this.cols; i++){
                    for(var j = 0; j < this.rows; j++){
                        this.state[i][j] = false;
                    }
                };
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

            applyDelta: function(delta){
                var self = this;
                delta.forEach(function(array) {
                    var x = array[1] + self.offsetX - 1;
                    var y = array[2] + self.offsetY - 1;

                    var point = erlife.viewport.translateToClient(x, y);
                    x = point[0];
                    y = point[1];
                    self.drawCell(x, y, array[0]);
                    self.state[x][y] = array[0];
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
                if (!erlife.isRunning){
                    erlife.canvas.mousedown = true;
                    var pos = erlife.canvas.mousePosition(e);
                    erlife.canvas.userState.set(pos.x, pos.y);
                    erlife.canvas.lastX = pos.x;
                    erlife.canvas.lastY = pos.y;
                }
            },

            onmousemove: function(e){
                if (erlife.canvas.mousedown){
                    var pos = erlife.canvas.mousePosition(e);
                    if (pos.x != erlife.canvas.lastX || pos.y != erlife.canvas.lastY){
                        erlife.canvas.userState.set(pos.x, pos.y);
                        erlife.canvas.lastX = pos.x;
                        erlife.canvas.lastY = pos.y;
                    }
                }
            },

            onmouseup: function(e){
                erlife.canvas.mousedown = false;
            }
        },

        viewport: {
            offsetX: 0,
            offsetY: 0,
            viewPortWidth: 100,
            viewPortHeight: 100,
            invalidate: false,

            scrollX: function(delta){
                this.offsetX += delta;
                this.invalidate = true;

                if (!erlife.isRunning){
                    erlife.server.getViewPort(this.getView());
                }
            },

            scrollY: function(delta){
                this.offsetY += delta;
                this.invalidate = true;

                if (!erlife.isRunning){
                    erlife.server.getViewPort(this.getView());
                }
            },

            translateToServer: function(x, y){
                return [x + this.offsetX, y + this.offsetY]
            },

            translateToClient: function(x, y){
                return [x - this.offsetX, y - this.offsetY]
            },

            getView: function(){
                return {
                    minX: this.offsetX - (this.viewPortWidth / 2),
                    maxX: this.offsetX + (this.viewPortWidth / 2),
                    minY: this.offsetY - (this.viewPortHeight / 2),
                    maxY: this.offsetY + (this.viewPortHeight / 2)
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

        saveState: function(id, name){
            var changesToState = this.canvas.userState.getState();
            this.canvas.userState.clear();
            this.server.saveState(id, name, changesToState);
        },

        loadState: function(id){
            if (!this.isRunning){
                this.canvas.clear();
                var viewport = this.viewport.getView();
                this.server.loadState(id, viewport);
            }
        },

        scrollX: function(delta){
            this.viewport.scrollX(delta);
        },

        scrollY: function(delta){
            this.viewport.scrollY(delta);
        },

        init: function(config){
            this.canvas.init(config.canvas, config.context);
            this.server.init(config.address, config.sessionId);
        }
    };
})()