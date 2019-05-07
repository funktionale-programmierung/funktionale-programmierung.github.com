
function sumArray(arr: number[]): number {
    let sum = 0;
    arr.forEach(x => {
        sum = sum + x;
    })
    return sum;
}

console.log(sumArray([1,2,3]));
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

function sumArray2(arr: (number | null | undefined)[] | null | undefined): number {
    if (!arr) {
        return 0;
    }
    let sum = 0;
    arr.forEach(x => {
        sum = sum + ((x === null || x === undefined) ? 0 : x);
    })
    return sum;
}

console.log(sumArray2([1,2,3]));
console.log(sumArray2([1,2,undefined]));
console.log(sumArray2(null));

interface Name {
    firstName: string;
    lastName: string;
}

interface Formatter {
    formatName: (name: Name | undefined) => string | undefined
}

function format(fmt: Formatter) {
    fmt.formatName(undefined);
}

const myFormatter = {
    formatName(name: Name): string {
        return name.firstName + " " + name.lastName;
    }
}

format(myFormatter);
