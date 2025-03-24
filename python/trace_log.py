import argparse
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib as mpl

def plot_deadlocks(data, plot=plt):
    column = 'deadlock'
    data = data[['size', column]].groupby('size', as_index=False)[column].agg(['mean', 'std']).reset_index()
    # data = data[['size', column]].groupby('size', as_index=False).mean()

    # plt.fill_between(data['size'], data['mean'] - data['std'], data['mean'] + data['std'], color='gray', alpha=0.05)
    plt.plot(data['size'], data['mean'] * 1000, color='gray', linestyle='--')

def bench_file(filepath, column="sent", name="", color="red", label=None, plot=plt):
    data = pd.read_csv(filepath, keep_default_na=False, comment="#")

#    if column == 'probes':
#        plot_deadlocks(data, plot=plot)

    data['service'] = data['mon_mon'] + data['proc_proc'] - data['probes']
    data['site'] = data['mon_mon'] + data['proc_proc']

    data = data[['size', column]].groupby('size', as_index=False)[column].agg(['mean', 'std']).reset_index()
    # data = data[['size', column]].groupby('size', as_index=False).mean()

    plt.fill_between(data['size'], data['mean'] - data['std'], data['mean'] + data['std'], color=color, alpha=0.1)
    plt.plot(data['size'], data['mean'], label=label, color=color)
    # plt.axhline(data['mean'].max(), color=color, linestyle="--", alpha=0.5, label=f"Max average = {data['mean'].max()} (at {data.loc[data['mean'].idxmax(), 'size']} processes)")

def bench(column, label=None, show=False, plot=plt):
    plt.figure(figsize=(7, 10), dpi=600)
    bench_file("bc_5000.csv", name="delayed (5s)", color="green", column=column, label="Probe delay = 5000ms", plot=plot)
    bench_file("bc_1000.csv", name="delayed (1s)", color="gold", column=column, label="Probe delay = 1000ms", plot=plot)
    bench_file("bc_500.csv", name="delayed (5s)", color="orange", column=column, label="Probe delay = 500ms", plot=plot)
    bench_file("bc_-1.csv", name="eager", color="red", column=column, label="Probe delay = 0", plot=plot)
    bench_file("bc_unmonitored.csv", name="unmonitored", color="blue", column=column, label="Unmonitored", plot=plot)

    # Labels and legend
    # plt.xlabel("Number of services")
    # plt.ylabel(f"Average {label}")
    # plt.legend(loc='upper left', bbox_to_anchor=(1, 1))

    plt.grid(axis='both', which='major')

    if show:
        plt.show()
    else:
        plt.savefig(f"benchmark_{column}.pdf", dpi=600, bbox_inches="tight")
        plt.close()

def hack_bench_legend():
    colors = ["green", "gold", "orange", "red", "blue"]
    labels = ["Probe delay = 5000ms", "Probe delay = 1000ms", "Probe delay = 500ms", "Probe delay = 0ms", "Unmonitored"]
    f = lambda m,c: plt.plot([],[],marker=m, color=c, ls="none")[0]
    handles = [f("s", colors[i]) for i in range(len(colors))]
    legend = plt.legend(handles, labels, loc=3, framealpha=1, frameon=False)

    fig  = legend.figure
    fig.canvas.draw()
    bbox  = legend.get_window_extent().transformed(fig.dpi_scale_trans.inverted())
    fig.savefig(f"benchmark_legend.pdf", dpi="figure", bbox_inches=bbox)
    plt.close()


def bench_bar(filepath, **kwargs):
    data = pd.read_csv(filepath, keep_default_na=False, comment="#")
    data['forward'] = data['mon_proc'] + data['proc_mon']

    labels = ['queries', 'replies', 'probes']

    sums = data.sum()
    bottom = 0
    for l in labels:
        plt.bar([0], [sums[l] / sums['sent']], label=l, bottom=bottom)
        bottom += sums[l]

def plot_data_type(data, val, min_range_ms=1300, max_range_ms=13000, **kwargs):
    cumcol = 'cum' + val
    data[cumcol] = ((data.data_type == val)  # Select message class
                    * (data.event_type == 'send')  # Only sent
                    * (data.who_type == data.other_type)  # Only same-class sender and receiver
                    ) + 0
    data[cumcol] = data[cumcol].cumsum()
    data = data[['timestamp', cumcol]]
    data = data[data.timestamp <= max_range_ms]
    if data.loc[len(data) - 1]['timestamp'] < min_range_ms:
        align_row = pd.DataFrame([[min_range_ms,data.loc[len(data) - 1][cumcol]]], columns=data.columns)
        data = pd.concat([data, align_row], ignore_index=True)
    plt.plot(data['timestamp'], data[cumcol], **kwargs)


def plot_states(data, state):
    for t in data[data.data_type == state]['timestamp']:
        plt.plot(t, data.loc[(data.timestamp - t).abs().idxmin()]['cumprobe'], 'rx', markersize=14)
        # plt.axvline(x=t, color='red', linestyle='--')


def ms_formatter(x, pos=None):
    return x.strftime('%H:%M:%S.') + f"{x.microsecond // 1000:03d}"


def timeseries(filepath, label=None, pcolor='orange', show=False):
    plt.figure(figsize=(10, 2), dpi=600)
    data = pd.read_csv(filepath, keep_default_na=False, comment="#")
    data['timestamp'] //= 1000  # To milliseconds
    # data['timestamp'] = pd.to_datetime(data['timestamp'], unit='us')

    plot_data_type(data, 'reply', label="Responses", color="cyan")
    plot_data_type(data, 'query', label="Queries", color="blue")
    plot_data_type(data, 'probe', label="Probes", color=pcolor)
    plot_states(data, 'deadlocked')

    # Labels and formatting
    # plt.xlabel('Timestamp')
    # plt.ylabel('Number of messages')
    # plt.title(label)
    plt.xticks(rotation=25)
    plt.gca().xaxis.set_major_formatter(mticker.FormatStrFormatter("%d ms"))
    # plt.legend(loc='upper left', bbox_to_anchor=(1, 1))

    plt.grid(axis='both', which='major')

    if show:
        plt.show()
    else:
        plt.savefig(Path(filepath).with_suffix(".pdf"), dpi=600, bbox_inches="tight")
        plt.close()


def gen_plots():
    bench('site', "messages sent between services")
    bench('sent', "all messages sent")
    bench('probes', "probes sent")
    bench('service', "queries and replies sent")
    hack_bench_legend()

    timeseries("ts_-1.csv", label="No probe delay; 20k services", pcolor='red')
    timeseries("ts_500.csv", label="Probe delay = 500ms; 20k services", pcolor='orange')
    timeseries("ts_1000.csv", label="Probe delay = 1000ms; 20k services", pcolor='gold')
    timeseries("ts_5000.csv", label="Probe delay = 5000ms; 20k services", pcolor='green')


def main():
    parser = argparse.ArgumentParser(description="This is serious enterprise software.")

    parser.add_argument('-t', type=str, help="Timeseries CSV file", required=False)
    parser.add_argument('-b', type=str, help="Benchmark CSV file", required=False)
    parser.add_argument('-s', type=str, help="Benchmark CSV file", required=False)

    # Parse arguments
    args = parser.parse_args()

    if args.t:
        timeseries(args.t, show=True)
    elif args.b:
        bench_file(args.b, 'service')
        bench_file(args.b, 'sent')
        bench_file(args.b, 'probes')
        plt.show()
    elif args.s:
        bench_bar(args.s)
        plt.show()
    else:
        gen_plots()


if __name__ == "__main__":
    main()
