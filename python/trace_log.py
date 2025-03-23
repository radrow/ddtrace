import argparse
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker

def bench_file(filepath, column="sent", name="", color="red", label=None, plot=plt):
    data = pd.read_csv(filepath, keep_default_na=False)

    data['service'] = data['mon_mon'] + data['proc_proc'] - data['probes']
    data['site'] = data['mon_mon'] + data['proc_proc']

    data = data[['size', column]].groupby('size', as_index=False)[column].agg(['mean', 'std']).reset_index()
    # data = data[['size', column]].groupby('size', as_index=False).mean()

    plt.fill_between(data['size'], data['mean'] - data['std'], data['mean'] + data['std'], color=color, alpha=0.1)
    plt.plot(data['size'], data['mean'], label=label, color=color)
    # plt.axhline(data['mean'].max(), color=color, linestyle="--", alpha=0.5, label=f"Max average = {data['mean'].max()} (at {data.loc[data['mean'].idxmax(), 'size']} processes)")

def bench(column, label=None, show=False, plot=plt):
    bench_file("benchmark_probe-delay_5000ms.csv", name="delayed (5s)", color="green", column=column, label="Probe delay = 5000ms", plot=plot)
    bench_file("benchmark_probe-delay_1000ms.csv", name="delayed (1s)", color="orange", column=column, label="Probe delay = 1000ms", plot=plot)
    # bench_file("benchmark_probe-delay_100ms.csv", name="delayed (100ms)", color="orange", column=column, label="Probe delay = 100ms", plot=plot)
    bench_file("benchmark_no-probe-delay.csv", name="eager", color="red", column=column, label="Probe delay = 0", plot=plot)
    bench_file("benchmark_unmonitored.csv", name="unmonitored", color="blue", column=column, label="Unmonitored", plot=plot)

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


def bench_bar(filepath, **kwargs):
    data = pd.read_csv(filepath, keep_default_na=False)
    data['forward'] = data['mon_proc'] + data['proc_mon']

    labels = ['queries', 'replies', 'probes']

    sums = data.sum()
    bottom = 0
    for l in labels:
        plt.bar([0], [sums[l] / sums['sent']], label=l, bottom=bottom)
        bottom += sums[l]

def plot_data_type(data, val, **kwargs):
    cumcol = 'cum' + val
    data[cumcol] = ((data.data_type == val)  # Select message class
                    * (data.event_type == 'send')  # Only sent
                    * (data.who_type == data.other_type)  # Only same-class sender and receiver
                    ) + 0
    data[cumcol] = data[cumcol].cumsum()
    data = data[['timestamp', cumcol]]
    plt.plot(data['timestamp'], data[cumcol], **kwargs)


def plot_states(data, state):
    for t in data[data.data_type == state]['timestamp']:
        plt.plot(t, data.loc[(data.timestamp - t).abs().idxmin()]['cumprobe'], 'ro', color='r')
        # plt.axvline(x=t, color='red', linestyle='--')


def ms_formatter(x, pos=None):
    return x.strftime('%H:%M:%S.') + f"{x.microsecond // 1000:03d}"


def timeseries(filepath, label=None, show=False):
    data = pd.read_csv(filepath, keep_default_na=False)
    data['timestamp'] //= 1000  # To milliseconds
    # data['timestamp'] = pd.to_datetime(data['timestamp'], unit='us')

    plot_data_type(data, 'query', label="Queries", color="b")

    plot_data_type(data, 'probe', label="Probes", color="orange")
    plot_states(data, 'deadlocked')

    plot_data_type(data, 'reply', label="Responses", color="g")

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

    timeseries("timeseries_no-probe-delay_20k-proc.csv", label="No probe delay; 20k services")
    timeseries("timeseries_probe-delay_500ms_20k-proc.csv", label="Probe delay = 500ms; 20k services")


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
