const canvas = document.getElementById("plane");
const ctx = canvas.getContext("2d");

function resizeCanvas() {
    const rect = canvas.getBoundingClientRect();

    canvas.width = rect.width;
    canvas.height = rect.height;
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

class Point2D {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    get X() {
        return this.x;
    }

    get Y() {
        return this.y;
    }

    GetDistanceTo(point) {
        return Math.hypot(point.X - this.X, point.Y - this.Y);
    }
}

function initPoints(n) {
    const margin = 50;
    const points = [];

    for (let i = 0; i < n; i++) {
        const x = Math.random() * (canvas.width - 2 * margin) + margin;
        const y = Math.random() * (canvas.height - 2 * margin) + margin;

        points.push(new Point2D(x, y));
    }

    return points;
}

function drawAxes(interval = 50) {
    ctx.strokeStyle = "rgba(0, 0, 0, 0.1)";
    ctx.lineWidth = 1;
    ctx.font = "13px sans-serif";
    ctx.fillStyle = "black";

    for (let x = 0; x <= canvas.width; x += interval) {
        ctx.beginPath();
        ctx.moveTo(x, 0);
        ctx.lineTo(x, canvas.height);
        ctx.stroke();
        ctx.fillText(`${x}`, x + 2, 12);
    }

    for (let y = 0; y <= canvas.height; y += interval) {
        ctx.beginPath();
        ctx.moveTo(0, y);
        ctx.lineTo(canvas.width, y);
        ctx.stroke();
        ctx.fillText(`${y}`, 2, y - 2);
    }
}

function drawPoints(points, highlight = {}) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawAxes();

    if (highlight.middleX !== undefined && highlight.d !== undefined) {
        ctx.fillStyle = 'rgba(0, 0, 255, 0.05)';
        ctx.fillRect(highlight.middleX - highlight.d, 0, 2 * highlight.d, canvas.height);

        ctx.strokeStyle = 'blue';
        ctx.beginPath();
        ctx.moveTo(highlight.middleX, 0);
        ctx.lineTo(highlight.middleX, canvas.height);
        ctx.stroke();
    }

    for (let i = 0; i < points.length; i++) {
        const p = points[i];

        ctx.beginPath();
        ctx.arc(p.X, p.Y, 8, 0, 2 * Math.PI);

        if (highlight.closestPair && highlight.closestPair.includes(p)) {
            ctx.fillStyle = 'blue';
        } else if (highlight.strip && highlight.strip.includes(p)) {
            ctx.fillStyle = 'yellow';
        } else if (highlight.range && i >= highlight.range[0] && i < highlight.range[1]) {
            ctx.fillStyle = 'green';
        } else {
            ctx.fillStyle = 'red';
        }

        ctx.fill();
    }

    if (highlight.bestLine) {
        ctx.beginPath();
        ctx.moveTo(highlight.bestLine[0].X, highlight.bestLine[0].Y);
        ctx.lineTo(highlight.bestLine[1].X, highlight.bestLine[1].Y);
        ctx.strokeStyle = 'green';
        ctx.lineWidth = 2;
        ctx.stroke();
        ctx.lineWidth = 1;
    }

    if (highlight.closestPair) {
        ctx.beginPath();
        ctx.moveTo(highlight.closestPair[0].X, highlight.closestPair[0].Y);
        ctx.lineTo(highlight.closestPair[1].X, highlight.closestPair[1].Y);
        ctx.strokeStyle = 'blue';
        ctx.lineWidth = 3;
        ctx.stroke();
        ctx.lineWidth = 1;
    }
}

async function closest(strip, d) {
    let min_distance = d;
    let closest_pair = [null, null];

    strip.sort((a, b) => a.Y - b.Y);

    for (let i = 0; i < strip.length; i++) {
        for (let j = i + 1; j < strip.length; j++) {
            if ((strip[j].Y - strip[i].Y) < min_distance) {
                const dist = strip[i].GetDistanceTo(strip[j]);
                if (dist < min_distance) {
                    min_distance = dist;
                    closest_pair = [strip[i], strip[j]];
                }

                ctx.beginPath();
                ctx.moveTo(strip[i].X, strip[i].Y);
                ctx.lineTo(strip[j].X, strip[j].Y);
                ctx.strokeStyle = 'rgba(0, 0, 0, 0.2)';
                ctx.stroke();
                await sleep(2000);
            } else {
                break;
            }
        }
    }

    return { dist: min_distance, pair: closest_pair };
}

async function find_min_distance(points, left, right) {
    drawPoints(points, { range: [left, right] });
    await sleep(2000);

    if ((right - left) <= 2) {
        let min_distance = Infinity;
        let min_pair = [null, null];

        for (let i = left; i < right; i++) {
            for (let j = i + 1; j < right; j++) {
                const dist = points[i].GetDistanceTo(points[j]);
                if (dist < min_distance) {
                    min_distance = dist;
                    min_pair = [points[i], points[j]];
                }

                ctx.beginPath();
                ctx.moveTo(points[i].X, points[i].Y);
                ctx.lineTo(points[j].X, points[j].Y);
                ctx.stroke();
                await sleep(2000);
            }
        }

        drawPoints(points, { range: [left, right], bestLine: min_pair });
        await sleep(2000);
        return { dist: min_distance, pair: min_pair };
    }

    const middle = Math.floor(left + (right - left) / 2);
    const middle_x = points[middle].X;

    const leftResult = await find_min_distance(points, left, middle);
    const rightResult = await find_min_distance(points, middle, right);

    let d = Math.min(leftResult.dist, rightResult.dist);
    let best_pair = leftResult.dist < rightResult.dist ? leftResult.pair : rightResult.pair;

    let strip = [];
    for (let i = left; i < right; i++) {
        if (Math.abs(points[i].X - middle_x) < d) {
            strip.push(points[i]);
        }
    }

    drawPoints(points, {
        range: [left, right],
        strip: strip,
        middleX: middle_x,
        d: d,
        bestLine: best_pair
    });

    await sleep(2000);

    const stripResult = await closest(strip, d);
    if (stripResult.dist < d) {
        return stripResult;
    }

    return { dist: d, pair: best_pair };
}

async function Start() {
    resizeCanvas();

    const points = initPoints(4);
    points.sort((a, b) => a.X - b.X);

    drawPoints(points);

    await sleep(2000);

    const result = await find_min_distance(points, 0, points.length, 0);
    drawPoints(points, { closestPair: result.pair });
}

window.addEventListener("resize", () => {
    Start();
});

Start();
