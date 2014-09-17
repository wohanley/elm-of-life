window.onload = function () {
    
    var randomGrid = function (width, height) {
        
        var randomRow = function (length) {
            
            var randomBoolean = function () {
                return Math.random() < 0.5;
            }

            var row = [];
            for (var i = 0; i < length; i++) {
                row.push(randomBoolean());
            }

            return row;
        }

        var grid = [];
        for (var i = 0; i < height; i++) {
            grid.push(randomRow(width));
        }

        return grid;
    }

    Elm.fullscreen(Elm.Conway, { initialBoard: randomGrid(30, 30) });
};
