(function(){
    var erlife = window.erlife = {
        isRunning: false,
        isInitialized: false,

        server: {
            bullet: null,

            init: function(address){
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
                    console.log(e.data);
                };

                this.bullet.onheartbeat = function(){
                    self.bullet.send('ping');
                }
            },

            start: function(userState){
                this.bullet.send($.toJSON({ command: "start", data: userState }));
            },

            nextGen: function(viewport, invalidate){
                this.bullet.send($.toJSON({
                                            command: "nextGen",
                                            data: {
                                                    viewport: [viewport.minX, viewport.minY, viewport.maxX, viewport.maxY],
                                                    invalidate: invalidate
                                                  }
                                          }));
            },

            stop: function(){
                this.bullet.send($.toJSON({ command: "stop" }));
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
                if (!this.isInitialized){
                    this.start();
                };

                //this.server.run(this.canvas.userState.getState());
                this.isRunning = true;
            }
        },

        pause: function(){
            if (this.isRunning){
                this.isRunning = false;
            }
        },

        nextGen: function(){
            if (!this.isRunning){
                if (!this.isInitialized){
                    this.start();
                }

                var viewport = this.viewport.getView();
                this.server.nextGen(viewport, this.viewport.invalidate);
            }
        },

        start: function(){
            if (!this.isInitialized){
                this.server.start(this.canvas.userState.getState());
                this.canvas.userState.clear();

                this.isInitialized = true;
            }
        },

        stop: function(){
            if (this.isInitialized){
                this.server.stop();
                this.canvas.draw();

                this.isInitialized = false;
            }

            this.isRunning = false;
        },

        init: function(config){
            this.canvas.init(config.canvas, config.context);
            this.server.init(config.address);
        }
    };
})()