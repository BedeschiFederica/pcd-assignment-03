package pcd.ass03.model;

import java.rmi.RemoteException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class DefaultGameStateManager implements GameStateManager {
    private static final double PLAYER_SPEED = 2.0;
    private static final Random random = new Random();
    private World world;
    private final Map<String, Position> playerDirections;
    private final List<WinnerListener> listeners;

    public DefaultGameStateManager(final World initialWorld) throws RemoteException {
        this.world = initialWorld;
        this.playerDirections = new HashMap<>();
        this.world.getPlayers().forEach(p -> {
            try {
                playerDirections.put(p.getId(), Position.ZERO);
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        });
        this.listeners = new ArrayList<>();
    }

    @Override
    public World getWorld() throws RemoteException {
        return this.world;
    }

    @Override
    public String addPlayer() throws RemoteException {
        final List<Integer> playerNumbers = this.world.getPlayers().stream().map(p -> {
            try {
                return Integer.valueOf((String) p.getId().subSequence(1, p.getId().length()));
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        }).toList();
        final int id =
                IntStream.iterate(1, n -> n + 1).dropWhile(playerNumbers::contains).findFirst().getAsInt();
        final String playerId = "p" + id;
        final Player player = new Player(playerId, random.nextInt(this.world.getWidth()),
                random.nextInt(this.world.getWidth()), Player.DEFAULT_MASS);
        this.world = new World(this.world.getWidth(), this.world.getHeight(),
                Stream.concat(this.world.getPlayers().stream(), Stream.of(player)).toList(), this.world.getFoods());
        return playerId;
    }

    @Override
    public void removePlayer(final String playerId) throws RemoteException {
        final Optional<Player> player = world.getPlayerById(playerId);
        if (player.isPresent()) {
            this.world = world.removePlayers(List.of(player.get()));
        }
    }

    @Override
    public void setPlayerDirection(final String playerId, final double dx, final double dy) throws RemoteException {
        // Ensure player exists before setting direction
        if (world.getPlayerById(playerId).isPresent()) {
            this.playerDirections.put(playerId, Position.of(dx, dy));
        }
    }

    @Override
    public void tick() throws RemoteException {
        this.world = handleEating(moveAllPlayers(this.world));
        cleanupPlayerDirections();
    }

    @Override
    public void addListener(final WinnerListener listener) throws RemoteException {
        this.listeners.add(listener);
    }

    @Override
    public void removeListener(final WinnerListener listener) throws RemoteException {
        this.listeners.remove(listener);
    }

    @Override
    public List<WinnerListener> getListeners() throws RemoteException {
        return this.listeners;
    }

    @Override
    public Optional<Player> getWinner() throws RemoteException {
        final double MASS_TO_WIN = 1000;
        return this.world.getPlayers().stream().filter(p -> {
            try {
                return p.getMass() > MASS_TO_WIN;
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        }).findAny();
    }

    private World moveAllPlayers(final World currentWorld) throws RemoteException {
        final List<Player> updatedPlayers = currentWorld.getPlayers().stream()
            .map(player -> {
                try {
                    Position direction = playerDirections.getOrDefault(player.getId(), Position.ZERO);
                    double newX = player.getX() + direction.x() * PLAYER_SPEED;
                    if (newX < 0 || newX > this.world.getWidth()) {
                        newX = player.getX();
                    }
                    double newY = player.getY() + direction.y() * PLAYER_SPEED;
                    if (newY < 0 || newY > this.world.getHeight()) {
                        newY = player.getY();
                    }
                    return player.moveTo(newX, newY);
                } catch (RemoteException e) {
                    throw new RuntimeException(e);
                }
            })
            .collect(Collectors.toList());

        return new World(currentWorld.getWidth(), currentWorld.getHeight(), updatedPlayers, currentWorld.getFoods());
    }

    private World handleEating(final World currentWorld) throws RemoteException {
        final List<Player> updatedPlayers = currentWorld.getPlayers().stream()
                .map(player -> {
                    try {
                        return growPlayer(currentWorld, player);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .toList();

        final List<Food> foodsToRemove = currentWorld.getPlayers().stream()
                .flatMap(player -> {
                    try {
                        return eatenFoods(currentWorld, player).stream();
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .distinct()
                .toList();

        final List<Player> playersToRemove = currentWorld.getPlayers().stream()
                .flatMap(player -> {
                    try {
                        return eatenPlayers(currentWorld, player).stream();
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .distinct()
                .toList();

        return new World(currentWorld.getWidth(), currentWorld.getHeight(), updatedPlayers, currentWorld.getFoods())
                .removeFoods(foodsToRemove)
                .removePlayers(playersToRemove);
    }

    private Player growPlayer(final World world, final Player player) throws RemoteException {
        final Player afterFood = eatenFoods(world, player).stream()
                .reduce(player, (p1, p2) -> {
                    try {
                        return p1.grow(p2);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                }, (p1, p2) -> p1);

        return eatenPlayers(world, afterFood).stream()
                .reduce(afterFood,  (p1, p2) -> {
                    try {
                        return p1.grow(p2);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                    }, (p1, p2) -> p1);
    }

    private List<Food> eatenFoods(final World world, final Player player) throws RemoteException {
        return world.getFoods().stream()
                .filter(food -> {
                    try {
                        return EatingManager.canEatFood(player, food);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .toList();
    }

    private List<Player> eatenPlayers(final World world, final Player player) throws RemoteException {
        return world.getPlayersExcludingSelf(player).stream()
                .filter(other -> {
                    try {
                        return EatingManager.canEatPlayer(player, other);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .toList();
    }

    private void cleanupPlayerDirections() throws RemoteException {
        List<String> currentPlayerIds = this.world.getPlayers().stream()
                .map(p -> {
                    try {
                        return p.getId();
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .collect(Collectors.toList());

        this.playerDirections.keySet().retainAll(currentPlayerIds);
        this.world.getPlayers().forEach(p ->
        {
            try {
                playerDirections.putIfAbsent(p.getId(), Position.ZERO);
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        });
    }

}
