#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>
#include <random>
#include <thread>

std::random_device rd;  // a seed source for the random number engine
std::mt19937 gen(rd()); // mersenne_twister_engine seeded with rd()

long d(long n) {
  std::uniform_int_distribution<> distrib(1, n);

  return distrib(gen);
}

class Histogram {
public:
  Histogram(long n_in) : distrib(1, n), n(n_in) { data.reset(new long[n]{0}); }

  long operator()() {
    long value = distrib(gen);
    ++data[value - 1];
    return value;
  }

  long operator()(long num) {
    long total = 0;
    for (long i = 0; i < num; ++i) {
      total += operator()();
    }
    return total;
  }

  Histogram &operator+=(const Histogram &other) {
    for (long i = 0; i < n; ++i) {
      data[i] += other.data[i];
    }

    return *this;
  }

  long Total() {
    long t = 0;
    for (long i = 0; i < n; ++i) {
      t += data[i];
    }
    return t;
  }

  void ComputeProbs() {
    double t = Total();
    prob.reset(new double[n]{0.f});
    for (long i = 0; i < n; ++i) {
      prob[i] = data[i] / t;
    }
  }

  double GetMean() {
    double t = 0;
    for (long i = 0; i < n; ++i) {
      t += prob[i] * (i + 1);
    }
    return t;
  }

  void DumpProbsForMathematica() {
    std::cout << "{";
    for (long i = 0; i < n; ++i) {
      std::cout << prob[i];
      if (i < n - 1) {
        std::cout << ",";
      }
    }
    std::cout << "}" << std::endl;
  }

  long n;
  std::uniform_int_distribution<> distrib;
  std::shared_ptr<long[]> data;
  std::shared_ptr<double[]> prob;
};

const bool critical_hit_rule = true;

bool battle(Histogram &hit_die, Histogram &damage_die) {
  long fighter_hp = 8;

  const long num_monsters = 1;
  long monster_hp = 100; // 40 + monster_damage_die(10);
  long to_hit_monster = 8;

  while (true) {
    // Fighter
    {
      long fighter_hit_roll = hit_die();
      if (fighter_hit_roll == 20 ||
          fighter_hit_roll >= to_hit_monster) {
        long fighter_damage = damage_die();
        if (critical_hit_rule && fighter_hit_roll == 20) {
          // critical hit
          fighter_damage += damage_die();
        }
        monster_hp -= fighter_damage;
        if (monster_hp <= 0) {
          // On to next monster
          break;
        }
      }
    }

  monster_start: {
    // Monster
    long monster_hit_roll = d(20);
    if (monster_hit_roll >= 10) {
      long monster_damage = d(6);
      fighter_hp -= monster_damage;
      if (fighter_hp <= 0) {
        return false;
      }
    }
  }
  }

  return true;
}

struct State {
  State() : count(0), wins(0), hit_die(20), damage_die(8) {}

  State &operator+=(const State &other) {
    count += other.count;
    wins += other.wins;
    hit_die += other.hit_die;
    damage_die += other.damage_die;
    return *this;
  }

  long count;
  long wins;
  Histogram hit_die;
  Histogram damage_die;
};

int main() {
  const long num_trials = 250'000'000;
  const int num_threads = 8;

  std::vector<std::thread> threads;

  State state[num_threads];
  for (int i = 0; i < num_threads; ++i) {
    threads.emplace_back([&state, i] {
      long last_percent = 0;
      for (long t = 0; t < num_trials; ++t) {
        long percent = 100L * long(t) / long(num_trials);
        if (percent > last_percent) {
          last_percent = percent;
          std::cout << i << ' ' << last_percent << "% \r" << std::flush;
        }

        State local_state;
        bool win = battle(local_state.hit_die, local_state.damage_die);
        local_state.wins = win;
        if (win) {
          state[i] += local_state;
        }
        ++state[i].count;
      }
    });
  }

  State total_state;
  for (int i = 0; i < num_threads; ++i) {
    threads[i].join();
    total_state += state[i];
  }

  std::cout << "Successes = " << total_state.wins << std::endl;
  std::cout << "Battles = " << total_state.count << std::endl;
  std::cout << "Win probability = "
            << double(total_state.wins) / double(total_state.count)
            << std::endl;

  total_state.hit_die.ComputeProbs();
  std::cout << "mean hit roll = " << total_state.hit_die.GetMean() << std::endl;
  total_state.hit_die.DumpProbsForMathematica();

  total_state.damage_die.ComputeProbs();
  std::cout << "mean damage roll = " << total_state.damage_die.GetMean()
            << std::endl;
  total_state.damage_die.DumpProbsForMathematica();
}
