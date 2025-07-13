namespace Pg

open Xunit
open Xunit.Abstractions
open Microsoft.EntityFrameworkCore

[<CLIMutable>]
type User = { id: int; name: string }

type AppDbContext(options: DbContextOptions<AppDbContext>) =
    inherit DbContext(options)

    [<DefaultValue>]
    val mutable private users0: DbSet<User>

    member this.users
        with get () = this.users0
        and set v = this.users0 <- v

// TODO: find something less opinionated
type PostgresConn(output: ITestOutputHelper) =

    // [<Fact>]
    [<Fact(Skip = "CI")>]
    let ``postgres simple connection`` () =
        let options =
            DbContextOptionsBuilder<AppDbContext>()
                .UseNpgsql("Host=localhost;Username=postgres;Password=pg123456;Database=fs")
                .Options

        use db = new AppDbContext(options)

        let r = db.users.Add({ id = 0; name = "Alex" })

        output.WriteLine($"{r}")

        db.SaveChanges() |> ignore

        Assert.True(true)
