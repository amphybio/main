import readline, subprocess, tempfile

def gnuplot(main_function, params, args, legends, plot_range=''):
    PLOT_CMD = ', '.join(['"{{data}}" using 1:{} title "{}"'.format(i+2, l) for i, l in enumerate(legends)])
    PLOT_CMD = 'set style data lines; set title "{title}"; plot ' + plot_range + ' ' + PLOT_CMD
    PLOT_TITLE = "Parameters:  " + "={}  ".join(params.split()) + "={}"

    with tempfile.NamedTemporaryFile(mode='w') as data:
        main_function(args, output=data)
        data.flush()
        command = ['gnuplot', '-p', '-e', PLOT_CMD.format(data=data.name, title=PLOT_TITLE.format(*args))]
        subprocess.run(command, stderr=subprocess.DEVNULL)

def input_gnuplot(main_function, params, legends):
    while True:
        try:
            args = input(params + " > ").split()
        except (EOFError, KeyboardInterrupt):
            return
        try:
            gnuplot(main_function, params, args, legends)
        except Exception as e:
            print("Error:", e)
