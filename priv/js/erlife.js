(function(){
    var erlife = window.erlife = {

        onUpdate: function(){
        },

        init: function(address){
                var bullet = $.bullet(address);

                bullet.onopen = function(){
                    console.log('bullet: opened');
                };

                bullet.ondisconnect = function(){
                    console.log('bullet: disconnected');
                };

                bullet.onclose = function(){
                    console.log('bullet: closed');
                };

                bullet.onmessage = function(e){
                    //alert(e.data);
                };

                bullet.onheartbeat = function(){
                    bullet.send('ping');
                }
            }
    };
})()
