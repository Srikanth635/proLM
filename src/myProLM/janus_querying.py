import janus_swi as janus


def demonstrate_query_differences():
    # Load the knowledge base
    janus.consult('janus_llm.pl')

    print("=== query_once() vs query() Demonstration ===\n")

    # Example 1: Getting movies by director
    print("1. Movies by Christopher Nolan:")
    print("   query_once() - First movie only:")
    first_nolan = janus.query_once("movie(Movie, nolan, Genre, Year)")
    print(f"   {first_nolan}")

    print("   query() - All movies:")
    all_nolan = list(janus.query("movie(Movie, nolan, Genre, Year)"))
    for movie in all_nolan:
        print(f"   {movie}")

    print("\n" + "=" * 50 + "\n")

    # Example 2: Getting all recommendations for inception
    print("2. Recommendations for 'inception':")
    print("   query_once() - First recommendation:")
    first_rec = janus.query_once(
        "recommendation_by_director(LikedMovie, Rec)",
        {'LikedMovie': 'inception'}
    )
    print(f"   {first_rec}")

    print("   query() - All recommendations:")
    all_recs = list(janus.query(
        "recommendation_by_director(LikedMovie, Rec)",
        {'LikedMovie': 'inception'}
    ))
    for rec in all_recs:
        print(f"   {rec}")

    print("\n" + "=" * 50 + "\n")

    # Example 3: Getting all movies (any director)
    print("3. All movies in database:")
    print("   Using query() to get all:")
    all_movies = list(janus.query("movie(Movie, Director, Genre, Year)"))
    for movie in all_movies:
        print(f"   {movie['Movie']} by {movie['Director']} ({movie['Year']})")

    print("\n" + "=" * 50 + "\n")

    # Example 4: Iterating without converting to list
    print("4. Iterating through results (memory efficient):")
    for movie in janus.query("movie(Movie, Director, Genre, Year)"):
        print(f"   Processing: {movie['Movie']}")

    print("\n" + "=" * 50 + "\n")

    # Example 5: Checking if something exists
    print("5. Existence checks:")
    exists = janus.query_once("movie(pulp_fiction, _, _, _)")
    print(f"   Does 'pulp_fiction' exist? {exists is not None}")

    not_exists = janus.query_once("movie(avatar, _, _, _)")
    print(f"   Does 'avatar' exist? {not_exists is not None}")

def test_gen_action_designator(prompt):
    """Function to be called from Prolog via janus"""
    output = {"cram_plan_response": prompt,
              "kb_file": "/home/malineni/PycharmProjects/ProLLM/src/logicLM/Logic-LLM/models/temp_outputs/kbst.pl"}
    return output

if __name__ == '__main__':
    demonstrate_query_differences()