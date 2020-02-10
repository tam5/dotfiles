"use strict";
exports.__esModule = true;
function max(arr) {
    var max = '';
    arr.forEach(function (item) {
        if (item.length > max.length) {
            max = item;
        }
    });
    return max;
}
exports.max = max;
//# sourceMappingURL=helpers.js.map