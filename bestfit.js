/**
 * 
 * @param {[number, number][]} data 
 * @param {object} options
 * 
 */
function findBestFit(data, equation = '$x + $', options = {}) {
    const { iterations, batchSize, format, minimumStepDp, precision, round, returnScore } = Object.assign({
        iterations: 1e2,
        minimumStepDp: 5,
        batchSize: 5e2,
        precision: 4,
        format: false,
        round: true,
        returnScore: false
    }, options);
    equation = parseMacros(equation);

    const minStep = 10 ** -minimumStepDp;

    // count the number of values to find ('$' character in string)  
    const values = (equation.match(/\$/g) || []).length;
    
    const INITIAL_STEP = .1;
    let step = INITIAL_STEP;

    let bestScore = Infinity;
    let bestValue = (new Array(values)).fill(1);
    let proportional = (new Array(values)).fill(true); // store which values are proportional

    let vi = 0;
    let batchIters = 0;
    let nextIterWeight = 1;
    let wrongDirection = 0;
    let wrongStep = 0;
    let direction = 1;

    // use precision + 1 to ensure accurate results
    const minimumWeight = 10 ** -(precision + 1);

    // pre generate custom scoring function
    const scoreReduce = new Function('data', 'bestValue', `
        let accumulator = 0;
        for(let i = 0; i < data.length; i++) {
            const current = data[i];
            const projected = ${parseEquation(equation, 'current[0]', 'bestValue')};
            accumulator += (current[1] - projected) ** 2;
        }
        return accumulator;
    `);

    bestScore = scoreReduce(data, bestValue);

    for(let i = 0; i < iterations * batchSize * values; i++) {
        batchIters += 1;
        if(batchIters >= batchSize) {
            vi = (vi + 1) % bestValue.length;
            batchIters = 0;
            step = INITIAL_STEP;
        }

        const originalValue = bestValue[vi];
        bestValue[vi] += direction * step * nextIterWeight;

        const score = scoreReduce(data, bestValue)

        nextIterWeight = proportional[vi] ? Math.sqrt(score / data.length) : 1;

        // if the new score is better, keep the changes
        if(score < bestScore) {
            bestScore = score;
            wrongDirection = 0;
            continue;
        }

        direction *= -1;
        bestValue[vi] = originalValue;

        // highest displayed precision already reached
        // break to prevent useless computation
        if(nextIterWeight < minimumWeight) break;

        // if both directions are wrong then this value is already as good 
        // as it can be for this batch
        wrongDirection++;
        if(wrongDirection !== 2) continue;

        step /= 10;
        wrongDirection = 0;
        wrongStep++;
        if(step > minStep) continue;

        // if all the dynamic step changes were wrong, the value is not proportional
        // in the next pass of this value, use non proportional mode
        if(wrongStep >= batchSize - 1) 
            proportional[vi] = false;

        wrongStep = 0;

        vi = (vi + 1) % bestValue.length;
        batchIters = 0;
        step = INITIAL_STEP;

    }

    if(format) {
        return formatEquation(equation, precision, bestValue);
    }

    // round return value to precision 
    if(round) {
        const roundingCoefficient = 10 ** precision;
        bestValue.forEach((v, i, arr) => arr[i] = Math.round((v + Number.EPSILON) * roundingCoefficient) / roundingCoefficient);
    }

    if(returnScore) return {score: bestScore, values: bestValue, proportional};
    return bestValue;
}

const equations = [
    '$/x + $',
    '$x + $',
    '$x^2 + $x + $',
    '$x^3 + $x^2 + $x + $',
    '$*sin($x) + $',
    '$*cos($x) + $',
    '$*tan($x) + $',
    '$^x'
];

function formatEquation(equation, precision, bestValue) {
    let printString = '';
    let idx = 0;
    for (let i = 0; i < equation.length; i++) {
        if (equation[i] === '$') {
            const roundingCoefficient = 10 ** precision;
            const value = Math.round((bestValue[idx] + Number.EPSILON) * roundingCoefficient) / roundingCoefficient;
            idx++;
            // simple coefficient detection to avoid cases like '1x'
            if (value === 1 && equation[i + 1] === 'x') continue;

            printString += String(value);
            continue;
        }

        printString += equation[i];
    }
    return printString;
}

function autoFindBestFit(data, format = false) {
    let bestScore = Infinity; // lower score is better
    let bestEquation;
    let bestValues = [];
    for(let i = 0; i < equations.length; i++) {
        const result = findBestFit(data, equations[i], {returnScore: true});

        // find score of rounded values
        const scoreReduce = new Function('data', 'bestValue', `
            let accumulator = 0;
            for(let i = 0; i < data.length; i++) {
                const current = data[i];
                const projected = ${parseEquation(equations[i], 'current[0]', 'bestValue')};
                accumulator += (current[1] - projected) ** 2;
            }
            return accumulator;
        `);
        result.score = scoreReduce(data, result.values);

        if(result.score >= bestScore) continue;
        bestEquation = equations[i];
        bestValues = result.values;
        bestScore = result.score;
    }

    if(format) return formatEquation(bestEquation, 2, bestValues);
    return [bestScore, bestEquation, bestValues];
}

function parseEquation(equation, xName = '$X', scoreName = '$SCORE') {
    equation = equation.replaceAll(' ', '');
    return _parseEquation(equation, xName, scoreName).equation;
}

/**
 * 
 * @param {string} equation 
 */
function _parseEquation(equation, xName = '$X', scoreName = '$SCORE', _IS_RECURSIVE = false, _idx = 0) {
    let idx = _idx;
    let result = '';

    const letter = /[a-zA-Z]/;
    const number = /[0-9.]/;
    const noMultiplication = ['+',',', '*', '/', '('];
    const operators = ['+', '*', '/'];
    let nesting = 0;

    for(let i = 0; i < equation.length; i++) {
        const c = equation[i];
        const prev = equation[i - 1] || '+';
        if(c === '$') {
            if(!noMultiplication.includes(prev)) result += '*';
            result += `${scoreName}[${idx}]`;
            idx++;
            
            continue
        }
        
        if(c.match(letter)) {
            let identifier = c;

            while(true) {
                if(equation[i + 1] === undefined || !equation[i+1].match(letter)) break;
                i++;

                identifier += equation[i];
            }

            if(identifier === 'x') {
                if(!noMultiplication.includes(prev)) result += '*';
                result += xName;
            } else if(identifier in Math && identifier !== 'toString') {
                if(!noMultiplication.includes(prev)) result += '*';

                // if this value is a constant like math or pi then just treat it as such
                if(typeof Math[identifier] === 'number') {
                    result += `Math.${identifier}`;
                    continue;
                }

                i++;
                if(equation[i] !== '(') throw new Error(`Parse Error: expected '(' after function call (encountered ${equation[i]})`);

                let nesting = 1;
                let exp = '';
                while(nesting > 0) {
                    i++;
                    if(equation[i] === undefined) {
                        if(nesting > 0) throw new Error('Unexpected end of input, unterminated paranthesis');
                        break;
                    }

                    if(equation[i] === '(') {
                        nesting++;
                        exp += '(';
                        continue;
                    }

                    if(equation[i] === ')') {
                        nesting--;
                        if(nesting >= 1) exp += ')';
                        continue;
                    }

                    exp += equation[i];
                }

                const res = _parseEquation(exp, xName, scoreName, true, idx);
                result += `Math.${identifier}(${res.equation})`;
                idx = res.idx;
            } else 
                throw new Error('unexpected identifier ' + identifier);
            
            continue;
        }

        if(c.match(number)) {
            result += c;
            continue;
        }

        // allow paranthesis for grouping
        if(['(', ')'].includes(c)) {
            nesting += c === '(' ? 1 : -1;
            result += c;
            continue;
        }
        
        if(operators.includes(c)) {
            // ** is valid however ^ can just be used instead
            if(operators.includes(prev)) throw new Error(`Invalid operator ${c} at ${i}`);
            result += c;
            continue;
        }

        if(c === ',') {
            if(_IS_RECURSIVE === false) throw new Error(`Parse Error: ',' is only allowed in function parameters but was encountered at ${i}`);
            result += ',';
            continue;
        }

        if(c === '^') {
            let num = '';

            // support exponential forms
            // use pow() for more complex expressions
            if(equation[i + 1] === 'x') {
                result += `**${xName}`;
                i++;
                continue;
            }

            if(equation[i + 1] === '$') {
                result += `**${scoreName}[${idx}]`;
                idx++;
                i++;
                continue;
            }

            let decimalFound = false;

            while(true) {
                const char = equation[i + 1];

                if(char === '.') {
                    if(decimalFound) throw new Error(`Parse Error: a number must only contain 1 decimal point`);

                    num += char;
                    decimalFound = true;
                }

                if(char === undefined || !char.match(/[0-9]/)) break;
                i++;

                num += char;
            }

            if(num.length === 0) throw new Error(`Parse Error: Expected number following exponent but encountered ${equation[i]} at ${i}`);
            
            result += `**${num}`;
            continue;
        }

        throw new Error(`Parse Error: unexpected character ${c} at ${i}`);
    }

    return {equation: result, idx};
}

const equationMacros = {
    poly(degree) {
        degree = parseInt(degree);
        if(isNaN(degree)) throw new Error(`Invalid degree '${degree}'`);
        if(degree < 1) throw new Error('degree must be greater than 1');
        let result = '';
        for(let i = degree; i >= 2; i--) {
            result += '$x^' + i + ' + ';
        }

        // base polynomial, degree = 1
        result += '$x + $';
        return result;
    }
}


function parseMacros(equation) {
    let result = '';
    for(let i = 0; i < equation.length; i++) {
        if(equation[i] !== '#') {
            result += equation[i];
            continue;
        }
        i++;

        let identifier = '';
        while(equation[i] !== undefined && equation[i].match(/[a-z]/)) {
            identifier += equation[i];
            i++;
        }

        if(equation[i] !== '(') throw new Error("Parse Error: expected '(' after macro call");

        let nesting = 1;
        let exp = '';
        while(nesting > 0) {
            i++;
            if(equation[i] === undefined) {
                if(nesting > 0) throw new Error('Unexpected end of input, unterminated paranthesis');
                break;
            }

            if(equation[i] === '(') {
                nesting++;
                continue;
            }

            if(equation[i] === ')') {
                nesting--;
                continue;
            }

            exp += equation[i];
        }

        if(equationMacros[identifier] === undefined) throw new Error(`No macro '${identifier}' found`)

        result += equationMacros[identifier](...exp.split(','));
    }

    return result;
}

// returns a closure which returns the corresponding value of y for x
function buildEvalFunction(equation, weights) {
    return new Function('weights', 'return x => ' + parseEquation(parseMacros(equation), 'x', 'weights'))(weights);
}

/**
 * convert 1d array into x, y pair
 * @param {Number[]} data 
 */
function sequential(data, start = 1) {
    const result = [];
    for(let i = start; i < start + data.length; i++) {
        result.push([i, data[i]]);
    }
    return result;
}