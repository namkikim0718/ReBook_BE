package com.be.rebook.members.service;

import com.be.rebook.members.dto.JoinDTO;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.repository.MembersRepository;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class JoinService {

    private  final MembersRepository membersRepository;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;

    public JoinService(MembersRepository membersRepository, BCryptPasswordEncoder bCryptPasswordEncoder){
        this.membersRepository = membersRepository;
        this.bCryptPasswordEncoder = bCryptPasswordEncoder;
    }
    public void joinProcess(JoinDTO joinDTO){
        String username = joinDTO.getUsername();
        String password = joinDTO.getPassword();

        Boolean isExist = membersRepository.existsByUsername(username);

        if(isExist){
            return;
        }

        Members data = new Members();
        data.setUsername(username);
        data.setPassword(bCryptPasswordEncoder.encode(password));
        data.setRole("ROLE_USER");

        membersRepository.save(data);
    }
}
