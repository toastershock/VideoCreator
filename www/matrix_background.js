const canvas = document.getElementById('canv');
        const ctx = canvas.getContext('2d');
    
        let cols = Math.floor(window.innerWidth / 20) + 1;
        let ypos = Array(cols).fill(0);
		let hue = 0;
        function matrix () {
            const w = window.innerWidth;
            const h = window.innerHeight;
        
            if (canvas.width !== w) {
                canvas.width = w;
                cols = Math.floor(window.innerWidth / 20) + 1;
                ypos = Array(cols).fill(0);
            }
            if (canvas.height !== h) {
                canvas.height = h;
            }
			// Gradually change the text color by updating the hue
			hue += 1;
			if (hue >= 360) {
			hue = 0;
			}
    
            ctx.fillStyle = '#0001';
            ctx.fillRect(0, 0, w, h);
    
            ctx.fillStyle = `hsl(${hue}, 100%, 50%)`; // Use HSL color with changing hue
            ctx.font = '15pt monospace';
    
            ypos.forEach((y, ind) => {
                const text = String.fromCharCode(Math.random() * 128);
                const x = ind * 20;
                ctx.fillText(text, x, y);
                if (y > 100 + Math.random() * 10000) ypos[ind] = 0;
                else ypos[ind] = y + 20;
            });
        }
    
setInterval(matrix, 50);
