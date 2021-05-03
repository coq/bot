open GitHub_types

(* Queries *)

module PullRequest_Milestone_and_Cards =
[%graphql
{|
  query backportInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        milestone @bsRecord {
          title
          description
        }
        projectCards(first:100) {
          nodes {
            id
            column @bsRecord {
              id
              databaseId
            }
            project {
              columns(first:100) {
                nodes @bsRecord {
                  id
                  databaseId
                }
              }
            }
          }
        }
      }
    }
  }
|}]

module PullRequest_ID_and_Milestone =
[%graphql
{|
  query prInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner,name: $repo) {
      pullRequest(number: $number) {
        id
        databaseId
        milestone {
          title
          description
        }
      }
    }
  }
|}]

module TeamMembership =
[%graphql
{|
  query teamMember($org: String!, $team: String!, $user: String!) {
    organization(login:$org) {
      team(slug:$team) {
        members(query:$user, first:1) {
          nodes { login }
        }
      }
    }
  }
|}]

module ParseAsString = struct
  let parse = Yojson.Basic.to_string

  let serialize = Yojson.Basic.from_string

  type t = string
end

module PullRequest_Refs =
[%graphql
{|
  query prRefs($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name:$repo) {
      pullRequest(number: $number) {
        id
        baseRefName
        baseRefOid @ppxCustom(module: "ParseAsString")
        headRefName
        headRefOid @ppxCustom(module: "ParseAsString")
        merged
        commits(last: 1) {
          nodes {
            commit {
              message
            }
          }
        }
      }
    }
  }
|}]

module Issue_Milestone =
[%graphql
{|
  query issueMilestone($owner: String!, $repo: String!, $number: Int!) {
    repository(owner:$owner, name:$repo) {
      issue(number:$number) {
        id
        milestone { id }
        timelineItems(itemTypes:[CLOSED_EVENT],last:1) {
          nodes {
            ... on ClosedEvent {
              closer {
                ... on PullRequest {
                  id
                  milestone { id }
                }
                ... on Commit {
                  associatedPullRequests(first: 2) {
                    nodes {
                      id
                      milestone { id }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
|}]

module PullRequestReviewsInfo =
[%graphql
{|
  query mergePullRequestInfo($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        baseRef {
          name
        }
        files(first: 100) {
          nodes {
            path
          }
        }
        comments(last:10) {
          nodes {
            id
            author {
              login
            }
            createdViaEmail
          }
        }
        reviewDecision
        latestReviews(first: 100) {
          nodes {
            author { login }
            state
          }
        }
        latestOpinionatedReviews(first: 100) {
          nodes {
            author { login }
            state
          }
        }
      }
    }
  }
|}]

module DefaultBranch =
[%graphql
{|
  query defaultBranch($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      defaultBranchRef {
        name
      }
    }
  }
|}]

module FileContent =
[%graphql
{|
  query fileContent($owner: String!, $repo: String!, $file: String!) {
    repository(owner: $owner, name: $repo) {
      file:object(expression: $file) {
        ... on Blob {
          text
        }
      }
    }
  }
|}]

module RepoId =
[%graphql
{|
  query repoId($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      id
    }
  }
|}]

(* Mutations *)

module MoveCardToColumn =
[%graphql
{|
  mutation moveCard($card_id:ID!,$column_id:ID!) {
    moveProjectCard(input:{cardId:$card_id,columnId:$column_id}) {
      clientMutationId
    }
  }
|}]

module PostComment =
[%graphql
{|
  mutation addComment($id:ID!,$message:String!) {
    payload: addComment(input:{subjectId:$id,body:$message}) {
      commentEdge {
        node {
          url @ppxCustom(module: "ParseAsString")
        }
      }
    }
  }
|}]

module UpdateMilestone =
[%graphql
{|
  mutation updateMilestone($issue: ID!, $milestone: ID!) {
    updateIssue(input: {id: $issue, milestoneId: $milestone}) {
      clientMutationId
    }
  }
|}]

module MergePullRequest =
[%graphql
{|
  mutation mergePullRequest($pr_id: ID!, $commit_headline: String,
  $commit_body: String, $merge_method: PullRequestMergeMethod) {
    mergePullRequest(
      input: {pullRequestId: $pr_id, commitHeadline: $commit_headline,
      commitBody: $commit_body, mergeMethod: $merge_method}) {
      pullRequest {
        merged
        mergedAt
        state
        url
      }
    }
  }
|}]

module NewCheckRun =
[%graphql
{|
  mutation newCheckRun($name: String!, $repoId: ID!, $headSha: String!,
  $status: RequestableCheckStatusState!, $title: String!, $text: String, $summary: String!,
  $url: String!, $conclusion: CheckConclusionState, $externalId: String) {
    createCheckRun(
      input: {
        status:$status,
        name:$name,
        repositoryId:$repoId,
        headSha:$headSha,
        conclusion:$conclusion,
        detailsUrl:$url,
        output:{
          title:$title,
          text:$text,
          summary:$summary
        }
        externalId:$externalId
      }) {
      clientMutationId
    }
  }
|}]

module UpdateCheckRun =
[%graphql
{|
  mutation updateCheckRun($checkRunId: ID!, $repoId: ID!
  $conclusion: CheckConclusionState!, $title: String!, $text: String,
  $url: String, $summary: String!) {
    updateCheckRun(
      input: {
        checkRunId:$checkRunId,
        repositoryId:$repoId,
        conclusion:$conclusion,
        detailsUrl:$url,
        output:{
          title:$title,
          text:$text,
          summary:$summary
        }
      }) {
      clientMutationId
    }
  }
|}]

module GetCheckRuns =
[%graphql
{|
  query getCheckRuns($appId: Int!, $owner: String!, $repo: String!, $commit: String!, $context: String!) {
    repository(owner:$owner, name:$repo) {
      obj: object(expression: $commit) {
        ... on Commit {
          checkSuites(first: 1, filterBy: { appId: $appId }) {
            nodes {
              checkRuns(first: 1, filterBy: { checkName: $context }) {
                nodes {
                  databaseId
                }
              }
            }
          }
          status {
            context(name: $context) {
              id
            }
          }
        }
      }
    }
  }
|}]

module GetBaseAndHeadChecks =
[%graphql
{|
fragment Checks on CheckSuiteConnection {
  nodes {
    checkRuns(first: 100) {
      nodes {
        name
        conclusion
        summary
        text
      }
    }
  }
}

query getChecks($appId: Int!, $owner: String!, $repo:String!, $prNumber: Int!, $base: String!, $head: String!) {
  repository(name: $repo,owner:$owner) {
    pullRequest(number: $prNumber) {
      id
      labels(first: 100) {
        nodes {
          name
        }
      }
      isDraft
    }
    base: object(expression: $base) {
      ... on Commit {
        checkSuites(first: 1,filterBy:{appId: $appId}) {
           ... Checks
        }
      }
    }
    head: object(expression: $head) {
      ... on Commit {
        checkSuites(first: 1,filterBy:{appId: $appId}) {
           ... Checks
        }
      }
    }
  }
}
|}]
