import * as argon2 from 'argon2';

class AuthService {
  public async SignUp(email, password, name): Promise<any> {
    const salt = randomBytes(32);
    const passwordHashed = await argon2.hash(password, { salt });

    const userRecord = await UserModel.create({
      password: passwordHashed,
      email,
      salt: salt.toString('hex'), // notice the .toString('hex')
      name,
    });
    return {
      // MAKE SURE TO NEVER SEND BACK THE PASSWORD OR SALT!!!!
      user: {
        email: userRecord.email,
        name: userRecord.name,
      },
    }
  }
}
