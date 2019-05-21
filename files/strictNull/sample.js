"use strict";
function sumArray(arr) {
    var sum = 0;
    arr.forEach(function (x) {
        sum = sum + x;
    });
    return sum;
}
console.log(sumArray([1, 2, 3]));
// console.log(sumArray([1,2,undefined]));
// console.log(sumArray(null));
/*
function sumArray2(arr: (number | null | undefined)[] | null | undefined): number {
    let sum = 0;
    arr.forEach(x => {
        sum = sum + x;
    })
    return sum;
}
*/
function sumArray2(arr) {
    if (!arr) {
        return 0;
    }
    var sum = 0;
    arr.forEach(function (x) {
        sum = sum + ((x === null || x === undefined) ? 0 : x);
    });
    return sum;
}
console.log(sumArray2([1, 2, 3]));
console.log(sumArray2([1, 2, undefined]));
console.log(sumArray2(null));
function format(fmt) {
    fmt.formatName(undefined);
}
var myFormatter = {
    formatName: function (name) {
        return name.firstName + " " + name.lastName;
    }
};
format(myFormatter);
