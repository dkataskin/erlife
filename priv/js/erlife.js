(function(){
    var erlife = window.erlife = {

        server: {
            bullet: undefined,

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
                    //alert(e.data);
                };

                this.bullet.onheartbeat = function(){
                    self.bullet.send('ping');
                }
            }
        },

        canvas: {
            context: null,
            canvas: null,
            width: 900,
            height: 500,
            cols: 100,
            rows: 100,
            cellSize: 8,
            cellSpace: 1,
            backgroundColor: '#B5B8B4',
            liveCellColor: '#28D18D',
            emptyCellColor: '#F3F3F3',
            mousedown: false,
            dragging: false,

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

            onmousedown: function(e){
                this.mousedown = true;
            },

            onmousemove: function(e){
                if (this.mousedown && !this.dragging){
                    this.dragging = true;
                }
            },

            onmouseup: function(e){
                this.mousedown = false;
                if (this.dragging){
                    this.dragging = false;
                }
            }
        },

        init: function(config){
            this.canvas.init(config.canvas, config.context);
            this.server.init(config.address);
        }
    };
})()