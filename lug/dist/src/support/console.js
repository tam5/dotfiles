"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
exports.__esModule = true;
var chalk_1 = __importDefault(require("chalk"));
function errorBlock(message) {
    var padding = ' ';
    var content = pad(message, padding);
    console.error();
    console.error(chalk_1["default"].bgRed(padding.repeat(content.length)));
    console.error(chalk_1["default"].bgRed(content));
    console.error(chalk_1["default"].bgRed(padding.repeat(content.length)));
    console.error();
}
exports.errorBlock = errorBlock;
function pad(message, delim, size) {
    if (delim === void 0) { delim = ' '; }
    if (size === void 0) { size = 8; }
    var padding = delim.repeat(size);
    return padding + message + padding;
}
//# sourceMappingURL=console.js.map