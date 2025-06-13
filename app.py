from flask import Flask, render_template, request, jsonify
import subprocess
import os

app = Flask(__name__)
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
COMPILER_DIR = os.path.join(BASE_DIR, "compiler")

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/compile', methods=['POST'])
def compile():
    code = request.form['code']
    
    # Ensure compiler directory exists
    os.makedirs(COMPILER_DIR, exist_ok=True)
    
    # Save input
    input_path = os.path.join(COMPILER_DIR, "input.c")
    with open(input_path, 'w') as f:
        f.write(code)
    
    try:
        # Run lexical analyzer
        lexer_src = os.path.join(COMPILER_DIR, "lexical_analyzer.c")
        lexer_bin = os.path.join(COMPILER_DIR, "lexer")
        subprocess.run(['gcc', lexer_src, '-o', lexer_bin], check=True)
        subprocess.run([lexer_bin], cwd=COMPILER_DIR, check=True)
        
        # Run syntax analyzer
        parser_src = os.path.join(COMPILER_DIR, "syntax_analyzer.c")
        parser_bin = os.path.join(COMPILER_DIR, "parser")
        subprocess.run(['gcc', parser_src, '-o', parser_bin], check=True)
        subprocess.run([parser_bin], cwd=COMPILER_DIR, check=True)
        
        # Run sementic analyzer
        semen_src = os.path.join(COMPILER_DIR, "semantic_analyzer.c")
        semen_bin = os.path.join(COMPILER_DIR, "sementic")
        subprocess.run(['gcc', semen_src, '-o', semen_bin], check=True)
        subprocess.run([semen_bin], cwd=COMPILER_DIR, check=True)
        
        # Read outputs
        output = {}
        for filename in ['tokens.txt', 'parse_tree.txt','symbol_table.txt']:
            path = os.path.join(COMPILER_DIR, filename)
            if os.path.exists(path):
                with open(path, 'r') as f:
                    output[filename.split('.')[0]] = f.read()
            else:
                output[filename.split('.')[0]] = f"Error: {filename} not generated"
        
        return jsonify(output)
    
    except subprocess.CalledProcessError as e:
        return jsonify({"error": f"Compilation failed at step: {e.cmd}"}), 500
    except Exception as e:
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')
