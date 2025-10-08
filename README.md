# 💰 Split Payment Contract

A Clarity smart contract for automated revenue sharing on the Stacks blockchain! Perfect for splitting payments among multiple recipients with customizable percentages.

## 🚀 Features

- ✨ Create multiple payment splits with custom names
- 👥 Add/remove recipients with specific percentage allocations
- 💸 Automatic payment distribution based on percentages
- 💳 Individual balance tracking and withdrawal system
- 🔧 Update recipient percentages dynamically
- ⏸️ Toggle split activation status
- 🔒 Owner-only controls for split management

## 📋 Contract Functions

### Public Functions

#### `create-split`
Creates a new payment split configuration
- **Parameters**: `name` (string-ascii 50)
- **Returns**: Split ID

#### `add-recipient`
Adds a recipient to an existing split
- **Parameters**: `split-id` (uint), `recipient` (principal), `percentage` (uint)
- **Requirements**: Only split creator, percentages must total ≤100%

#### `remove-recipient`
Removes a recipient from a split
- **Parameters**: `split-id` (uint), `recipient` (principal)
- **Requirements**: Only split creator

#### `send-payment`
Sends payment to be distributed among recipients
- **Parameters**: `split-id` (uint), `amount` (uint)
- **Requirements**: Split must be active, percentages must total 100%

#### `withdraw-balance`
Allows users to withdraw their accumulated balance
- **Requirements**: Must have positive balance

#### `toggle-split-status`
Activates/deactivates a payment split
- **Parameters**: `split-id` (uint)
- **Requirements**: Only split creator

#### `update-recipient-percentage`
Updates a recipient's percentage allocation
- **Parameters**: `split-id` (uint), `recipient` (principal), `new-percentage` (uint)
- **Requirements**: Only split creator, total percentages ≤100%

### Read-Only Functions

#### `get-split-info`
Returns information about a specific split
- **Parameters**: `split-id` (uint)

#### `get-recipient-info`
Returns recipient information for a specific split
- **Parameters**: `split-id` (uint), `recipient` (principal)

#### `get-user-balance`
Returns the withdrawable balance for a user
- **Parameters**: `user` (principal)

#### `get-next-split-id`
Returns the next available split ID

## 🛠️ Usage Example

### Deploy and Test

```bash
clarinet console
```

### Create a Split
```clarity
(contract-call? .split-payment-contract create-split "Team Revenue Split")
```

### Add Recipients
```clarity
(contract-call? .split-payment-contract add-recipient u1 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE u30)
(contract-call? .split-payment-contract add-recipient u1 'ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK u70)
```

### Send Payment
```clarity
(contract-call? .split-payment-contract send-payment u1 u1000000)
```

### Withdraw Balance
```clarity
(contract-call? .split-payment-contract withdraw-balance)
```

## 🔍 Error Codes

- `u100`: Owner only operation
- `u101`: Split/recipient not found
- `u102`: Invalid percentage allocation
- `u103`: Recipient already exists
- `u104`: Insufficient balance
- `u105`: Invalid recipient (cannot be creator)
- `u106`: No recipients configured
- `u107`: Payment distribution failed

## 🎯 Use Cases

- 💼 Business partnership revenue sharing
- 🎵 Music royalty distribution
- 🎮 Gaming tournament prize splits
- 💡 Project contributor payments
- 🏢 Affiliate commission distribution

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

## 📝 License

MIT License - feel free to use this contract for your revenue sharing needs!

## 🤝 Contributing

Contributions welcome! Please feel free to submit a Pull Request.
