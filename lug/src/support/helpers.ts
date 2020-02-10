/**
 * Get largest item in a list.
 */
export function max(arr: string[]) {
    let max = '';

    arr.forEach(item => {
        if (item.length > max.length) {
            max = item
        }
    })

    return max
}
